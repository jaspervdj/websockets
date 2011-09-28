-- | Provides a simple, clean monad to write websocket servers in
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Network.WebSockets.Monad
    ( WebSocketsOptions (..)
    , defaultWebSocketsOptions
    , WebSockets (..)
    , runWebSockets
    , runWebSocketsWith
    , receive
    , send
    , Sender
    , getSender
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Monad (replicateM)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadIO, lift, liftIO)
import System.Random (randomRIO)

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Data.Attoparsec (Parser)
import Data.Attoparsec.Enumerator (iterParser)
import Data.ByteString (ByteString)
import Data.Enumerator ( Iteratee, Stream (..), checkContinue0, isEOF, returnI
                       , run, ($$), (>>==)
                       )
import qualified Data.ByteString as B

import Network.WebSockets.Demultiplex (DemultiplexState, emptyDemultiplexState)
import Network.WebSockets.Encode (Encoder)

-- | Options for the WebSocket program
data WebSocketsOptions = WebSocketsOptions
    { onPong :: IO ()
    }

-- | Default options
defaultWebSocketsOptions :: WebSocketsOptions
defaultWebSocketsOptions = WebSocketsOptions
    { onPong = return ()
    }

-- | Environment in which the 'WebSockets' monad actually runs
data WebSocketsEnv = WebSocketsEnv WebSocketsOptions (Builder -> IO ())

-- | The monad in which you can write WebSocket-capable applications
newtype WebSockets a = WebSockets
    { unWebSockets :: ReaderT WebSocketsEnv
        (StateT DemultiplexState (Iteratee ByteString IO)) a
    } deriving (Functor, Monad, MonadIO)

-- | Run a 'WebSockets' application on an 'Enumerator'/'Iteratee' pair.
runWebSockets :: WebSockets a
              -> Iteratee ByteString IO ()
              -> Iteratee ByteString IO a
runWebSockets = runWebSocketsWith defaultWebSocketsOptions

-- | Version of 'runWebSockets' which allows you to specify custom options
runWebSocketsWith :: WebSocketsOptions
                  -> WebSockets a
                  -> Iteratee ByteString IO ()
                  -> Iteratee ByteString IO a
runWebSocketsWith options ws outIter = do
    sendLock <- liftIO $ newMVar () 
    let env   = WebSocketsEnv options (makeSend sendLock)
        state = runReaderT (unWebSockets ws) env
        iter  = evalStateT state emptyDemultiplexState 
    iter
  where
    makeSend sendLock x = do
        () <- takeMVar sendLock
        _ <- run $ singleton x $$ builderToByteString $$ outIter
        putMVar sendLock ()

    singleton c = checkContinue0 $ \_ f -> f (Chunks [c]) >>== returnI

-- | Receive some data from the socket, using a user-supplied parser.
receive :: Parser a -> WebSockets (Maybe a)
receive parser = WebSockets $ lift $ lift $ do
    eof <- isEOF
    if eof then return Nothing else fmap Just (iterParser parser)

-- | Low-level sending with an arbitrary 'Encoder'
send :: Encoder a -> a -> WebSockets ()
send encoder x = do
    sender <- getSender
    liftIO $ sender encoder x

-- | For asynchronous sending
type Sender a = Encoder a -> a -> IO ()

-- | In case the user of the library wants to do asynchronous sending to the
-- socket, he can extract a 'Sender' and pass this value around, for example,
-- to other threads.
getSender :: WebSockets (Sender a)
getSender = WebSockets $ do
    WebSocketsEnv _ send' <- ask
    return $ \encoder x -> do
        bytes <- replicateM 4 (liftIO randomByte)
        send' (encoder (Just (B.pack bytes)) x)
  where
    randomByte = fromIntegral <$> randomRIO (0x00 :: Int, 0xff)
