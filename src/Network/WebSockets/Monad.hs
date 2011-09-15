-- | Provides a simple, clean monad to write websocket servers in
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.WebSockets.Monad
    ( WebSockets
    , runWebSockets
    , receive
    , send
    , Sender
    , getSender
    ) where

import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Exception (SomeException)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (MonadIO, lift, liftIO)

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Enumerator (builderToByteString)
import Data.Attoparsec (Parser)
import Data.Attoparsec.Enumerator (iterParser)
import Data.ByteString (ByteString)
import Data.Enumerator ( Enumerator, Iteratee, Stream (..), checkContinue0
                       , isEOF, returnI, run, ($$), (>>==)
                       )

import Network.WebSockets.Encode (Encoder)

newtype WebSockets a = WebSockets
    { unWebSockets :: ReaderT
        (Builder -> IO ()) (Iteratee ByteString IO) a
    } deriving (Functor, Monad, MonadIO)

runWebSockets :: WebSockets a
              -> Enumerator ByteString IO a
              -> Iteratee ByteString IO ()
              -> IO (Either SomeException a)
runWebSockets ws inEnum outIter = do
    sendLock <- newMVar () 
    let inIter = runReaderT (unWebSockets ws) (makeSend sendLock)
    run (inEnum $$ inIter)
  where
    makeSend sendLock x = do
        () <- takeMVar sendLock
        _ <- run $ singleton x $$ builderToByteString $$ outIter
        putMVar sendLock ()

    singleton c = checkContinue0 $ \_ f -> f (Chunks [c]) >>== returnI

receive :: Parser a -> WebSockets (Maybe a)
receive parser = WebSockets $ lift $ do
    eof <- isEOF
    if eof then return Nothing else fmap Just (iterParser parser)

-- | Low-level sending with an arbitrary 'Encoder'
send :: Encoder a -> a -> WebSockets ()
send encoder x = do
    sender <- getSender
    liftIO $ sender encoder x

-- | For asynchronous sending
type Sender a = Encoder a -> a -> IO ()

getSender :: WebSockets (Sender a)
getSender = WebSockets $ do
    send' <- ask
    return $ \encoder x -> send' (encoder x)
