-- | Provides a simple, clean monad to write websocket servers in
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Network.WebSockets.Monad
    ( WebSockets (..)
    , runWebSockets
    , receive
    , send
    , Sender
    , getSender
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Exception (SomeException)
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
import Data.Enumerator ( Enumerator, Iteratee, Stream (..), checkContinue0
                       , isEOF, returnI, run, ($$), (>>==)
                       )
import qualified Data.ByteString as B

import Network.WebSockets.Demultiplex (DemultiplexState, emptyDemultiplexState)
import Network.WebSockets.Encode (Encoder)

-- | The monad in which you can write WebSocket-capable applications
newtype WebSockets a = WebSockets
    { unWebSockets :: ReaderT (Builder -> IO ())
        (StateT DemultiplexState (Iteratee ByteString IO)) a
    } deriving (Functor, Monad, MonadIO)

-- | Run a 'WebSockets' application on an 'Enumerator'/'Iteratee' pair.
runWebSockets :: WebSockets a
              -> Enumerator ByteString IO a
              -> Iteratee ByteString IO ()
              -> IO (Either SomeException a)
runWebSockets ws inEnum outIter = do
    sendLock <- newMVar () 
    let state  = runReaderT (unWebSockets ws) (makeSend sendLock)
        inIter = evalStateT state emptyDemultiplexState 
    run (inEnum $$ inIter)
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
    send' <- ask
    return $ \encoder x -> do
        bytes <- replicateM 4 (liftIO randomByte)
        send' (encoder (Just (B.pack bytes)) x)
  where
    randomByte = fromIntegral <$> randomRIO (0x00 :: Int, 0xff)
