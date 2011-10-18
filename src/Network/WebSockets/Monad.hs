-- | Provides a simple, clean monad to write websocket servers in
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings,
        NoMonomorphismRestriction, Rank2Types, ScopedTypeVariables #-}
module Network.WebSockets.Monad
    ( WebSocketsOptions (..)
    , defaultWebSocketsOptions
    , WebSockets (..)
    , runWebSockets
    , runWebSocketsWith
    , runWebSocketsHandshake
    , runWebSocketsWithHandshake
    , runWebSocketsWith'
    , receiveWith
    , sendWith
    , send
    , Sink
    , sendSink
    , getSink
    , getOptions
    , getProtocol
    , getVersion
    , throwWsError
    , catchWsError
    , spawnPingThread
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (Exception (..), SomeException, throw)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.Trans (MonadIO, lift, liftIO)

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Data.ByteString (ByteString)
import Data.Enumerator (Enumerator, Iteratee, ($$), (>>==))
import qualified Data.Attoparsec.Enumerator as AE
import qualified Data.Enumerator as E

import Network.WebSockets.Demultiplex (DemultiplexState, emptyDemultiplexState)
import Network.WebSockets.Handshake
import Network.WebSockets.Http
import Network.WebSockets.Mask
import Network.WebSockets.Protocol
import Network.WebSockets.ShyIterParser
import Network.WebSockets.Types as T
import qualified Network.WebSockets.Encode as E

-- | Options for the WebSocket program
data WebSocketsOptions = WebSocketsOptions
    { onPong       :: IO ()
    }

-- | Default options
defaultWebSocketsOptions :: WebSocketsOptions
defaultWebSocketsOptions = WebSocketsOptions
    { onPong       = return ()
    }

-- | Environment in which the 'WebSockets' monad actually runs
data WebSocketsEnv p = WebSocketsEnv
    { options     :: WebSocketsOptions
    , sendBuilder :: Builder -> IO ()
    , protocol    :: p
    }

-- | The monad in which you can write WebSocket-capable applications
newtype WebSockets p a = WebSockets
    { unWebSockets :: ReaderT (WebSocketsEnv p)
        (StateT DemultiplexState (Iteratee ByteString IO)) a
    } deriving (Functor, Monad, MonadIO)

-- | Receives the initial client handshake, then behaves like 'runWebSockets'.
runWebSocketsHandshake :: Protocol p
     => (Request -> WebSockets p a)
     -> Iteratee ByteString IO ()
     -> Iteratee ByteString IO a
runWebSocketsHandshake = runWebSocketsWithHandshake defaultWebSocketsOptions

-- | Receives the initial client handshake, then behaves like
-- 'runWebSocketsWith.
runWebSocketsWithHandshake :: Protocol p
                           => WebSocketsOptions
                           -> (Request -> WebSockets p a)
                           -> Iteratee ByteString IO ()
                           -> Iteratee ByteString IO a
runWebSocketsWithHandshake opts goWs outIter = do
    httpReq <- receiveIteratee decodeRequest
    runWebSocketsWith opts httpReq goWs outIter

-- | Run a 'WebSockets' application on an 'Enumerator'/'Iteratee' pair, given
-- that you (read: your web server) has already received the HTTP part of the
-- initial request. If not, you might want to use 'runWebSocketsWithHandshake'
-- instead.
--
-- If the handshake failed, returns HandshakeError. Otherwise, executes the
-- supplied continuation. If you want to accept the connection, you should send
-- the included 'requestResponse', and a 'response400' otherwise.
runWebSockets :: Protocol p
              => RequestHttpPart
              -> (Request -> WebSockets p a)
              -> Iteratee ByteString IO ()
              -> Iteratee ByteString IO a
runWebSockets = runWebSocketsWith defaultWebSocketsOptions

-- | Version of 'runWebSockets' which allows you to specify custom options
runWebSocketsWith :: forall p a.
       Protocol p
    => WebSocketsOptions
    -> RequestHttpPart
    -> (Request -> WebSockets p a)
    -> Iteratee ByteString IO ()
    -> Iteratee ByteString IO a
runWebSocketsWith opts httpReq goWs outIter = do
    let impls = implementations :: [p]
    mreq <- receiveIterateeShy $ tryFinishRequest httpReq
    case mreq of
        (Left err) -> do
            sendIteratee encodeResponse (responseError impls err) outIter
            E.throwError err
        (Right (r, p)) -> runWebSocketsWith' opts p (goWs r) outIter

runWebSocketsWith' :: Protocol p
                   => WebSocketsOptions
                   -> p
                   -> WebSockets p a
                   -> Iteratee ByteString IO ()
                   -> Iteratee ByteString IO a
runWebSocketsWith' opts proto ws outIter = do
    sendLock <- liftIO $ newMVar () 

    let sender = makeSend sendLock
        env    = WebSocketsEnv opts sender proto
        state  = runReaderT (unWebSockets ws) env
        iter   = evalStateT state emptyDemultiplexState
    iter
  where
    makeSend sendLock x = withMVar sendLock $ \_ ->
        builderSender outIter x

-- | @spawnPingThread n@ spawns a thread which sends a ping every @n@ seconds
-- (if the protocol supports it). To be called after having sent the response.
spawnPingThread :: BinaryProtocol p => Int -> WebSockets p ()
spawnPingThread i = do
    sink <- getSink
    _ <- liftIO $ forkIO $ forever $ do
        sendSink sink $ ping ("Hi" :: ByteString)
        threadDelay (i * 1000 * 1000)  -- seconds
    return ()

-- | Receive some data from the socket, using a user-supplied parser.
receiveWith :: Decoder p a -> WebSockets p a
receiveWith = liftIteratee . receiveIteratee

-- todo: move some stuff to another module. "Decode"?

-- | Underlying iteratee version of 'receiveWith'.
receiveIteratee :: Decoder p a -> Iteratee ByteString IO a
receiveIteratee parser = do
    eof <- E.isEOF
    if eof
        then E.throwError ConnectionClosed
        else wrappingParseError . AE.iterParser $ parser

-- | Like receiveIteratee, but if the supplied parser is happy with no input,
-- we don't supply any more. This is very, very important when we have parsers
-- that don't necessarily read data, like hybi10's completeRequest.
receiveIterateeShy :: Decoder p a -> Iteratee ByteString IO a
receiveIterateeShy parser = wrappingParseError $ shyIterParser parser

-- | Execute an iteratee, wrapping attoparsec-enumeratee's ParseError into the
-- ParseError constructor (which is a ConnectionError).
wrappingParseError :: (Monad m) => Iteratee a m b -> Iteratee a m b
wrappingParseError = flip E.catchError $ \e -> E.throwError $
    maybe e (toException . ParseError) $ fromException e

sendIteratee :: Encoder p a -> a -> Iteratee ByteString IO () -> Iteratee ByteString IO ()
sendIteratee enc resp outIter = do
    liftIO $ mkSend (builderSender outIter) enc resp

-- | Low-leven sending with an arbitrary 'Encoder'
sendWith :: Encoder p a -> a -> WebSockets p ()
sendWith encoder x = WebSockets $ do
    send' <- sendBuilder <$> ask
    liftIO $ mkSend send' encoder x

-- | Low-level sending with an arbitrary 'T.Message'
send :: Protocol p => T.Message p -> WebSockets p ()
send msg = getSink >>= \sink -> liftIO $ sendSink sink msg

-- | Used for asynchronous sending.
newtype Sink p = Sink {unSink :: Message p -> IO ()}

-- | Send a message to a sink. Might generate an exception if the underlying
-- connection is closed.
sendSink :: Sink p -> Message p -> IO ()
sendSink = unSink

-- | In case the user of the library wants to do asynchronous sending to the
-- socket, he can extract a 'Sink' and pass this value around, for example,
-- to other threads.
getSink :: Protocol p => WebSockets p (Sink p)
getSink = WebSockets $ do
    proto <- unWebSockets getProtocol
    send' <- sendBuilder <$> ask
    let encodeMsg = E.message (encodeFrame proto)
    return $ Sink $ mkSend send' encodeMsg

-- TODO: rename to mkEncodedSender?
mkSend :: (Builder -> IO ()) -> Encoder p a -> a -> IO ()
mkSend send' encoder x = do
    mask <- randomMask
    send' $ encoder mask x

singleton :: Monad m => a -> Enumerator a m b
singleton c = E.checkContinue0 $ \_ f -> f (E.Chunks [c]) >>== E.returnI

builderSender :: MonadIO m => Iteratee ByteString m b -> Builder -> m ()
builderSender outIter x = do
    ok <- E.run $ singleton x $$ builderToByteString $$ outIter
    case ok of
        Left err -> throw err
        Right _  -> return ()

-- | Get the current configuration
getOptions :: WebSockets p WebSocketsOptions
getOptions = WebSockets $ ask >>= return . options

-- | Get the underlying protocol
getProtocol :: WebSockets p p
getProtocol = WebSockets $ protocol <$> ask

-- | Find out the 'WebSockets' version used at runtime
getVersion :: Protocol p => WebSockets p String
getVersion = version <$> getProtocol

-- | Throw an iteratee error in the WebSockets monad
throwWsError :: (Exception e) => e -> WebSockets p a
throwWsError = liftIteratee . E.throwError

-- | Catch an iteratee error in the WebSockets monad
catchWsError :: WebSockets p a -> (SomeException -> WebSockets p a) -> WebSockets p a
catchWsError act c = WebSockets $ do
    env <- ask
    state <- get
    let it  = peelWebSockets state env $ act
        cit = peelWebSockets state env . c
    lift . lift $ it `E.catchError` cit
    where peelWebSockets state env =
            flip evalStateT state . flip runReaderT env . unWebSockets

-- | Lift an Iteratee computation to WebSockets
liftIteratee :: Iteratee ByteString IO a -> WebSockets p a
liftIteratee = WebSockets . lift . lift
