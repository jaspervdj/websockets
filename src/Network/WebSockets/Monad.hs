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
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, replicateM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import System.Random (randomRIO)

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Attoparsec (parse, Parser(..), Result(..))
import Data.ByteString (ByteString)
import Data.Enumerator ( Enumerator, Iteratee, Stream (..), checkContinue0, isEOF, returnI
                       , run, ($$), (>>==)
                       )
import qualified Data.Enumerator as E
import qualified Data.ByteString as B

import Network.WebSockets.Decode (Decoder, request)
import Network.WebSockets.Demultiplex (DemultiplexState, emptyDemultiplexState)
import Network.WebSockets.Encode (Encoder)
import qualified Network.WebSockets.Encode as E
import Network.WebSockets.Types as T

import Network.WebSockets.Handshake

-- | Options for the WebSocket program
data WebSocketsOptions = WebSocketsOptions
    { onPong       :: IO ()
    , pingInterval :: Maybe Int
    }

-- | Default options
defaultWebSocketsOptions :: WebSocketsOptions
defaultWebSocketsOptions = WebSocketsOptions
    { onPong       = return ()
    -- , pingInterval = Just 10
    -- TODO: reset this and don't spawn the ping thread on -00
    -- (otherwise it will instantly crash with -00 because we don't
    -- have pings there)
    , pingInterval = Nothing
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
     -> Iteratee ByteString IO (Either HandshakeError a)
runWebSocketsHandshake = runWebSocketsWithHandshake defaultWebSocketsOptions

-- | Receives the initial client handshake, then behaves like
-- 'runWebSocketsWith.
runWebSocketsWithHandshake :: WebSocketsOptions
                  -> (Request -> WebSockets a)
                  -> Iteratee ByteString IO ()
                  -> Iteratee ByteString IO (Either HandshakeError a)
runWebSocketsWithHandshake opts goWs outIter = do
    httpReq <- receiveIteratee request
    case httpReq of
        Nothing -> return . Left $ OtherError "unexpected EOF"  -- todo: should behave like a parse error!
        Just r -> runWebSocketsWith opts r goWs outIter

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
              -> Iteratee ByteString IO (Either HandshakeError a)
runWebSockets = runWebSocketsWith defaultWebSocketsOptions

-- | Version of 'runWebSockets' which allows you to specify custom options
runWebSocketsWith ::
       WebSocketsOptions
    -> RequestHttpPart
    -> (Request -> WebSockets a)
    -> Iteratee ByteString IO ()
    -> Iteratee ByteString IO (Either HandshakeError a)
runWebSocketsWith opts httpReq goWs outIter = do
    mreq <- receiveIteratee' $ tryFinishRequest httpReq
    case mreq of
        Nothing -> return . Left $ OtherError "unexpected EOF"  -- todo: should behave like a parse error!
                                                                -- (see receiveIteratee)
        Just (Left err) -> do
            sendIteratee E.response (responseError err) outIter
            return (Left err)
        Just (Right (r, p)) -> Right `fmap` runWebSocketsWith' opts p (goWs r) outIter

runWebSocketsWith' :: WebSocketsOptions
                  -> Protocol
                  -> WebSockets a
                  -> Iteratee ByteString IO ()
                  -> Iteratee ByteString IO a
runWebSocketsWith' opts proto ws outIter = do
    sendLock <- liftIO $ newMVar () 

    let sender = makeSend sendLock
        env    = WebSocketsEnv opts sender proto
        state  = runReaderT (unWebSockets ws') env
        iter   = evalStateT state emptyDemultiplexState
    iter
  where
    makeSend sendLock x = withMVar sendLock $ \_ ->
        builderSender outIter x

    -- Spawn a ping thread first
    ws' = spawnPingThread >> ws

-- | Spawn a thread which sends a ping every few seconds, according to the
-- options set
spawnPingThread :: WebSockets ()
spawnPingThread = do
    sender <- getMessageSender
    opts <- getOptions
    case pingInterval opts of
        Nothing -> return ()
        Just i  -> do
            _ <- liftIO $ forkIO $ forever $ do
                sender $ T.ping ("Hi" :: ByteString)
                threadDelay (i * 1000 * 1000)  -- seconds
            return ()

-- | Receive some data from the socket, using a user-supplied parser.
receive :: Decoder a -> WebSockets (Maybe a)
receive = WebSockets . lift . lift . receiveIteratee

-- ^ todo: again, why does this have a Maybe type? It won't do what the user
-- expects!

-- todo: move some stuff to another module. "Decode"?

-- TODO: WRONG for parsers that possibly don't take any data (= only validate). Also the isEOF.
receiveIteratee :: Decoder a -> Iteratee ByteString IO (Maybe a)
receiveIteratee parser = do
    eof <- isEOF
    if eof then return Nothing else fmap Just (iterParser parser)

-- | Like receiveIteratee, but if the supplied parser is happy with no input,
-- we don't supply any more. This is very, very important when we have parsers
-- that don't necessarily read data, like hybi10's completeRequest.
--
-- todo: Does this function not yet existing mean we're abusing attoparsec?
receiveIteratee' :: Decoder a -> Iteratee ByteString IO (Maybe a)
receiveIteratee' parser = trace "receiveIteratee'" $
    -- todo: we are redoing part of the parse. Which is not good. In fact, we
    -- should be re-using a Partial result.
    case parse parser "" of
        Done _ a -> return (Just a)
        _ -> fmap Just $ iterParser parser

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
