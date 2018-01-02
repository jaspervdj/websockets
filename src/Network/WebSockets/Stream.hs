--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
{-# LANGUAGE CPP #-}
module Network.WebSockets.Stream
    ( Stream
    , makeStream
    , makeSocketStream
    , makeEchoStream
    , parse
    , parseBin
    , write
    , close
    ) where

import           Control.Concurrent.MVar        (MVar, newEmptyMVar, newMVar,
                                                 putMVar, takeMVar, withMVar)
import           Control.Exception              (onException, throwIO)
import           Control.Monad                  (forM_)
import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.Binary.Get                as BIN
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
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

import           Network.WebSockets.Types


--------------------------------------------------------------------------------
-- | State of the stream
data StreamState
    = Closed !B.ByteString  -- Remainder
    | Open   !B.ByteString  -- Buffer


--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
data Stream = Stream
    { streamIn    :: IO (Maybe B.ByteString)
    , streamOut   :: (Maybe BL.ByteString -> IO ())
    , streamState :: !(IORef StreamState)
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
    return $ Stream (receive' ref receiveLock) (send' ref sendLock) ref
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
        mbBs <- onException receive (closeRef ref)
        case mbBs of
            Nothing -> closeRef ref >> return Nothing
            Just bs -> return (Just bs)

    send' :: IORef StreamState -> MVar () -> (Maybe BL.ByteString -> IO ())
    send' ref lock mbBs = withMVar lock $ \() -> do
        case mbBs of
            Nothing -> closeRef ref
            Just _  -> assertOpen ref
        onException (send mbBs) (closeRef ref)


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
