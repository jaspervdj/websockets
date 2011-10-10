-- | Provides a simple, clean monad to write websocket servers in
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Monad
    ( WebSocketsOptions (..)
    , defaultWebSocketsOptions
    , WebSockets (..)
    , runWebSockets
    , runWebSocketsWith
    , runWebSocketsHandshake
    , runWebSocketsWithHandshake
    , receive
    , sendMessage
    , getMessageSender
    , getSender
    , getOptions
    , getProtocol
    , throwWsError
    , catchWsError
    , spawnPingThread
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import Control.Exception.Base
import System.Random (randomRIO)

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Data.Attoparsec.Enumerator (iterParser)
import qualified Data.Attoparsec.Enumerator as AE
import Data.Attoparsec (parse, Parser(..), Result(..))
import Data.ByteString (ByteString)
import Data.Enumerator ( Enumerator, Iteratee, Stream (..), checkContinue0, isEOF, returnI
                       , run, ($$), (>>==), throwError
                       )
import qualified Data.Enumerator as E
import qualified Data.ByteString as B

import Network.WebSockets.Decode (Decoder, request)
import Network.WebSockets.Demultiplex (DemultiplexState, emptyDemultiplexState)
import Network.WebSockets.Encode (Encoder)
import qualified Network.WebSockets.Encode as E
import Network.WebSockets.Types as T

import Network.WebSockets.Handshake

import Network.WebSockets.ShyIterParser

import Network.WebSockets.Feature (Feature)
import qualified Network.WebSockets.Feature as F
import Data.List (sort, isPrefixOf)

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
data WebSocketsEnv = WebSocketsEnv
    { options     :: WebSocketsOptions
    , sendBuilder :: Builder -> IO ()
    , protocol    :: Protocol
    }

-- | The monad in which you can write WebSocket-capable applications
newtype WebSockets a = WebSockets
    { unWebSockets :: ReaderT WebSocketsEnv
        (StateT DemultiplexState (Iteratee ByteString IO)) a
    } deriving (Functor, Monad, MonadIO)

-- | Receives the initial client handshake, then behaves like 'runWebSockets'.
runWebSocketsHandshake :: 
        (Request -> WebSockets a)
     -> Iteratee ByteString IO ()
     -> Iteratee ByteString IO a
runWebSocketsHandshake = runWebSocketsWithHandshake defaultWebSocketsOptions

-- | Receives the initial client handshake, then behaves like
-- 'runWebSocketsWith.
runWebSocketsWithHandshake :: WebSocketsOptions
                  -> (Request -> WebSockets a)
                  -> Iteratee ByteString IO ()
                  -> Iteratee ByteString IO a
runWebSocketsWithHandshake opts goWs outIter = do
    httpReq <- receiveIteratee request
    runWebSocketsWith opts httpReq goWs outIter

-- | Run a 'WebSockets' application on an 'Enumerator'/'Iteratee' pair, given
-- that you (read: your web server) has already received the HTTP part of the
-- initial request. If not, you might want to use 'runWebSocketsWithHandshake'
-- instead.
--
-- If the handshake failed, returns HandshakeError. Otherwise, executes the
-- supplied continuation. If you want to accept the connection, you should send
-- the included 'requestResponse', and a 'response400' otherwise.
runWebSockets :: RequestHttpPart
              -> (Request -> WebSockets a)
              -> Iteratee ByteString IO ()
              -> Iteratee ByteString IO a
runWebSockets = runWebSocketsWith defaultWebSocketsOptions

-- | Version of 'runWebSockets' which allows you to specify custom options
runWebSocketsWith ::
       WebSocketsOptions
    -> RequestHttpPart
    -> (Request -> WebSockets a)
    -> Iteratee ByteString IO ()
    -> Iteratee ByteString IO a
runWebSocketsWith opts httpReq goWs outIter = do
    mreq <- receiveIterateeShy $ tryFinishRequest httpReq
    case mreq of
        (Left err) -> do
            sendIteratee E.response (responseError err) outIter
            E.throwError err
        (Right (r, p)) -> runWebSocketsWith' opts p (goWs r) outIter

runWebSocketsWith' :: WebSocketsOptions
                  -> Protocol
                  -> WebSockets a
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
spawnPingThread :: Int -> WebSockets ()
spawnPingThread i = hasFeatures F.ping >>= when `flip` do
    sender <- getMessageSender
    _ <- liftIO $ forkIO $ forever $ do
        sender $ T.ping ("Hi" :: ByteString)
        threadDelay (i * 1000 * 1000)  -- seconds
    return ()

-- | Receive some data from the socket, using a user-supplied parser.
receive :: Decoder a -> WebSockets a
receive = liftIteratee . receiveIteratee

-- todo: move some stuff to another module. "Decode"?

-- | Underlying iteratee version of 'receive'.
receiveIteratee :: Decoder a -> Iteratee ByteString IO a
receiveIteratee parser = do
    eof <- isEOF
    if eof
        then E.throwError ConnectionClosed
        else wrappingParseError . iterParser $ parser

-- | Like receiveIteratee, but if the supplied parser is happy with no input,
-- we don't supply any more. This is very, very important when we have parsers
-- that don't necessarily read data, like hybi10's completeRequest.
receiveIterateeShy :: Decoder a -> Iteratee ByteString IO a
receiveIterateeShy parser = wrappingParseError $ shyIterParser parser

-- | Execute an iteratee, wrapping attoparsec-enumeratee's ParseError into the
-- ParseError constructor (which is a ConnectionError).
wrappingParseError :: (Monad m) => Iteratee a m b -> Iteratee a m b
wrappingParseError = flip E.catchError $ \e -> E.throwError $
    maybe e (toException . ParseError) $ fromException e

sendIteratee :: Encoder a -> a -> Iteratee ByteString IO () -> Iteratee ByteString IO ()
sendIteratee enc resp outIter = do
    liftIO $ mkSender (builderSender outIter) enc resp

-- | Low-level sending with an arbitrary 'T.Message'
sendMessage :: T.Message -> WebSockets ()
sendMessage msg = getMessageSender >>= (liftIO . ($ msg))

-- | In case the user of the library wants to do asynchronous sending to the
-- socket, he can extract a 'Sender' and pass this value around, for example,
-- to other threads.
getMessageSender :: WebSockets (T.Message -> IO ())
getMessageSender = do
    proto <- getProtocol
    let encodeMsg = E.message (encodeFrame proto)
    getSender encodeMsg

getSender :: Encoder a -> WebSockets (a -> IO ())
getSender encoder = WebSockets $ do
    send' <- sendBuilder <$> ask
    return $ mkSender send' encoder

mkSender :: (Builder -> IO ()) -> Encoder a -> a -> IO ()
mkSender send' encoder x = do
    bytes <- replicateM 4 (liftIO randomByte)
    send' (encoder (Just (B.pack bytes)) x)
  where
    randomByte = fromIntegral <$> randomRIO (0x00 :: Int, 0xff)

singleton :: Monad m => a -> Enumerator a m b
singleton c = checkContinue0 $ \_ f -> f (Chunks [c]) >>== returnI

builderSender :: MonadIO m => Iteratee ByteString m b -> Builder -> m ()
builderSender outIter x = (run $ singleton x $$ builderToByteString $$ outIter) >> return ()

-- | Get the current configuration
getOptions :: WebSockets WebSocketsOptions
getOptions = WebSockets $ ask >>= return . options

-- | Get the underlying protocol
getProtocol :: WebSockets Protocol
getProtocol = WebSockets $ ask >>= return . protocol

-- | Check if the given features are supported with the current protocol.
hasFeatures :: F.Features -> WebSockets Bool
hasFeatures fs = (fs `F.isSubsetOf`) . features <$> getProtocol

-- | Throw an iteratee error in the WebSockets monad
throwWsError :: (Exception e) => e -> WebSockets a
throwWsError = liftIteratee . throwError

-- | Catch an iteratee error in the WebSockets monad
catchWsError :: WebSockets a -> (SomeException -> WebSockets a) -> WebSockets a
catchWsError act c = WebSockets $ do
    env <- ask
    state <- get
    let it  = peelWebSockets state env $ act
        cit = peelWebSockets state env . c
    lift . lift $ it `E.catchError` cit
    where peelWebSockets state env =
            flip evalStateT state . flip runReaderT env . unWebSockets

-- | Lift an Iteratee computation to WebSockets
liftIteratee :: Iteratee ByteString IO a -> WebSockets a
liftIteratee = WebSockets . lift . lift

