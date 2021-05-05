--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Network.WebSockets.Stream
    ( Stream
    , makeStream
    , makeSocketStream
    , makeEchoStream
    , parse
    , parseBin
    , write
    , close
    -- * TLS
    , makeTlsSocketStream
    , streamTlsContext
    ) where

import           Control.Applicative            ((<|>))
import           Control.Concurrent.MVar        (MVar, newEmptyMVar, newMVar,
                                                 putMVar, takeMVar, withMVar)
import           Control.Exception              (SomeException, SomeAsyncException, catch, handle, throwIO, fromException)
import           Control.Monad                  (forM_)
import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.Binary.Get                as BIN
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Default.Class             (def)
import           Data.Functor                   ((<&>))
import qualified Data.IORef                     as IO
import           Data.IORef                     (IORef, atomicModifyIORef',
                                                 newIORef, readIORef,
                                                 writeIORef)
import qualified Network.Socket                 as S
import qualified Network.Socket.ByteString      as SB (recv)

#if !defined(mingw32_HOST_OS)
import qualified Network.Socket.ByteString.Lazy as SBL (sendAll)
#else
import qualified Network.Socket.ByteString      as SB (sendAll)
#endif

import qualified Network.TLS                as TLS
import qualified Network.TLS.SessionManager as SM
import           Network.WebSockets.Types
import           Network.WebSockets.Connection.Options
import           System.IO.Error                (isEOFError)


--------------------------------------------------------------------------------
-- | State of the stream
data StreamState
    = Closed !B.ByteString  -- Remainder
    | Open   !B.ByteString  -- Buffer


--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
data Stream = Stream
    { streamIn         :: IO (Maybe B.ByteString)
    , streamOut        :: (Maybe BL.ByteString -> IO ())
    , streamState      :: !(IORef StreamState)
    , streamTlsContext :: Maybe TLS.Context
    }

--------------------------------------------------------------------------------
-- | Create a stream from a "receive" and "send" action. The following
-- properties apply:
--
-- - Regardless of the provided "receive" and "send" functions, reading and
--   writing from the stream will be thread-safe, i.e. this function will create
--   a receive and write lock to be used internally.
--
-- - Reading from or writing or to a closed 'Stream' will always throw an
--   exception, even if the underlying "receive" and "send" functions do not
--   (we do the bookkeeping).
--
-- - Streams should always be closed.
makeStream
    :: IO (Maybe B.ByteString)         -- ^ Reading
    -> (Maybe BL.ByteString -> IO ())  -- ^ Writing
    -> IO Stream                       -- ^ Resulting stream
makeStream receive send = do
    ref         <- newIORef (Open B.empty)
    receiveLock <- newMVar ()
    sendLock    <- newMVar ()
    return $ Stream (receive' ref receiveLock) (send' ref sendLock) ref Nothing
  where
    closeRef :: IORef StreamState -> IO ()
    closeRef ref = atomicModifyIORef' ref $ \state -> case state of
        Open   buf -> (Closed buf, ())
        Closed buf -> (Closed buf, ())

    -- Throw a 'ConnectionClosed' is the connection is not 'Open'.
    assertOpen :: IORef StreamState -> IO ()
    assertOpen ref = do
        state <- readIORef ref
        case state of
            Closed _ -> throwIO ConnectionClosed
            Open   _ -> return ()

    receive' :: IORef StreamState -> MVar () -> IO (Maybe B.ByteString)
    receive' ref lock = withMVar lock $ \() -> do
        assertOpen ref
        mbBs <- onSyncException receive (closeRef ref)
        case mbBs of
            Nothing -> closeRef ref >> return Nothing
            Just bs -> return (Just bs)

    send' :: IORef StreamState -> MVar () -> (Maybe BL.ByteString -> IO ())
    send' ref lock mbBs = withMVar lock $ \() -> do
        case mbBs of
            Nothing -> closeRef ref
            Just _  -> assertOpen ref
        onSyncException (send mbBs) (closeRef ref)

    onSyncException :: IO a -> IO b -> IO a
    onSyncException io what =
        catch io $ \e -> do
            case fromException (e :: SomeException) :: Maybe SomeAsyncException of
                Just _  -> pure ()
                Nothing -> what *> pure ()
            throwIO e

--------------------------------------------------------------------------------
makeSocketStream :: S.Socket -> IO Stream
makeSocketStream socket = makeStream receive send
  where
    receive = do
        bs <- SB.recv socket 8192
        return $ if B.null bs then Nothing else Just bs

    send Nothing   = return ()
    send (Just bs) = do
#if !defined(mingw32_HOST_OS)
        SBL.sendAll socket bs
#else
        forM_ (BL.toChunks bs) (SB.sendAll socket)
#endif

loadCredentials :: TLSSettings -> IO TLS.Credentials
loadCredentials TLSSettings{ tlsCredentials = Just creds } = return creds
loadCredentials TLSSettings{..} = case certSettings of
  CertFromFile cert chainFiles key -> do
    cred <- either error id <$> TLS.credentialLoadX509Chain cert chainFiles key
    return $ TLS.Credentials [cred]
  CertFromRef certRef chainCertsRef keyRef -> do
    cert <- IO.readIORef certRef
    chainCerts <- mapM IO.readIORef chainCertsRef
    key <- IO.readIORef keyRef
    cred <- either error return $ TLS.credentialLoadX509ChainFromMemory cert chainCerts key
    return $ TLS.Credentials [cred]
  CertFromMemory certMemory chainCertsMemory keyMemory -> do
    cred <- either error return $ TLS.credentialLoadX509ChainFromMemory certMemory chainCertsMemory keyMemory
    return $ TLS.Credentials [cred]

makeTlsSocketStream :: TLSSettings -> S.Socket -> IO Stream
makeTlsSocketStream stts socket = do
  creds <- loadCredentials stts
  mgr <- getSessionManager stts
  ctx <- TLS.contextNew socket (params mgr creds)
  TLS.contextHookSetLogging ctx (tlsLogging stts)
  TLS.handshake ctx
  makeStream (receive ctx) (send ctx) <&>
    \s -> s { streamTlsContext = Just ctx }
 where
    receive ctx = handle onEOF go
     where
       onEOF e
         | Just TLS.Error_EOF <- fromException e       = pure Nothing
         | Just ioe <- fromException e, isEOFError ioe = pure Nothing
         | otherwise                                   = throwIO e
       go = do
           x <- TLS.recvData ctx
           if B.null x then
               go
             else
               pure $ Just x

    send _ Nothing   = return ()
    send ctx (Just bs) =
      TLS.sendData ctx bs

    params mgr creds = def { -- TLS.ServerParams
        TLS.serverWantClientCert = tlsWantClientCert stts
      , TLS.serverCACertificates = []
      , TLS.serverDHEParams      = tlsServerDHEParams stts
      , TLS.serverHooks          = hooks
      , TLS.serverShared         = shared mgr creds
      , TLS.serverSupported      = supported
      , TLS.serverEarlyDataSize  = 2018
      }
    -- Adding alpn to user's tlsServerHooks.
    hooks = (tlsServerHooks stts)
      { TLS.onALPNClientSuggest  = TLS.onALPNClientSuggest (tlsServerHooks stts)
      --  <|> (if settingsHTTP2Enabled set then Just alpn else Nothing)
      }

    shared mgr creds = def {
        TLS.sharedCredentials    = creds
      , TLS.sharedSessionManager = mgr
      }
    supported = def { -- TLS.Supported
        TLS.supportedVersions            = tlsAllowedVersions stts
      , TLS.supportedCiphers             = tlsCiphers stts
      , TLS.supportedCompressions        = [TLS.nullCompression]
      , TLS.supportedSecureRenegotiation = True
      , TLS.supportedClientInitiatedRenegotiation = False
      , TLS.supportedSession             = True
      , TLS.supportedFallbackScsv        = True
      , TLS.supportedGroups              = [TLS.X25519,TLS.P256,TLS.P384]
      }

    getSessionManager :: TLSSettings -> IO TLS.SessionManager
    getSessionManager TLSSettings{ tlsSessionManager = Just mgr } = return mgr
    getSessionManager stts' = case tlsSessionManagerConfig stts' of
      Nothing     -> return TLS.noSessionManager
      Just config -> SM.newSessionManager config


--------------------------------------------------------------------------------
makeEchoStream :: IO Stream
makeEchoStream = do
    mvar <- newEmptyMVar
    makeStream (takeMVar mvar) $ \mbBs -> case mbBs of
        Nothing -> putMVar mvar Nothing
        Just bs -> forM_ (BL.toChunks bs) $ \c -> putMVar mvar (Just c)


--------------------------------------------------------------------------------
parseBin :: Stream -> BIN.Get a -> IO (Maybe a)
parseBin stream parser = do
    state <- readIORef (streamState stream)
    case state of
        Closed remainder
            | B.null remainder -> return Nothing
            | otherwise        -> go (BIN.runGetIncremental parser `BIN.pushChunk` remainder) True
        Open buffer
            | B.null buffer -> do
                mbBs <- streamIn stream
                case mbBs of
                    Nothing -> do
                        writeIORef (streamState stream) (Closed B.empty)
                        return Nothing
                    Just bs -> go (BIN.runGetIncremental parser `BIN.pushChunk` bs) False
            | otherwise     -> go (BIN.runGetIncremental parser `BIN.pushChunk` buffer) False
  where
    -- Buffer is empty when entering this function.
    go (BIN.Done remainder _ x) closed = do
        writeIORef (streamState stream) $
            if closed then Closed remainder else Open remainder
        return (Just x)
    go (BIN.Partial f) closed
        | closed    = go (f Nothing) True
        | otherwise = do
            mbBs <- streamIn stream
            case mbBs of
                Nothing -> go (f Nothing) True
                Just bs -> go (f (Just bs)) False
    go (BIN.Fail _ _ err) _ = throwIO (ParseException err)


parse :: Stream -> Atto.Parser a -> IO (Maybe a)
parse stream parser = do
    state <- readIORef (streamState stream)
    case state of
        Closed remainder
            | B.null remainder -> return Nothing
            | otherwise        -> go (Atto.parse parser remainder) True
        Open buffer
            | B.null buffer -> do
                mbBs <- streamIn stream
                case mbBs of
                    Nothing -> do
                        writeIORef (streamState stream) (Closed B.empty)
                        return Nothing
                    Just bs -> go (Atto.parse parser bs) False
            | otherwise     -> go (Atto.parse parser buffer) False
  where
    -- Buffer is empty when entering this function.
    go (Atto.Done remainder x) closed = do
        writeIORef (streamState stream) $
            if closed then Closed remainder else Open remainder
        return (Just x)
    go (Atto.Partial f) closed
        | closed    = go (f B.empty) True
        | otherwise = do
            mbBs <- streamIn stream
            case mbBs of
                Nothing -> go (f B.empty) True
                Just bs -> go (f bs) False
    go (Atto.Fail _ _ err) _ = throwIO (ParseException err)


--------------------------------------------------------------------------------
write :: Stream -> BL.ByteString -> IO ()
write stream = streamOut stream . Just


--------------------------------------------------------------------------------
close :: Stream -> IO ()
close stream = streamOut stream Nothing
