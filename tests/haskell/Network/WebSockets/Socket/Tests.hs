{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Socket.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever, forM_, replicateM)
import Control.Monad.Trans (liftIO)

import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee, ($$))
import Data.Text (Text)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, arbitrary)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator as E
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB

import Network.WebSockets
import Network.WebSockets.Monad
import Network.WebSockets.Socket
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi00
import Network.WebSockets.Protocol.Hybi10
import Network.WebSockets.Tests.Util

tests :: Test
tests = testGroup "Network.WebSockets.Socket.Tests"
    [ testProperty "sendReceive-hybi10" (sendReceive Hybi10_)
    , testProperty "sendReceive-hybi00" (sendReceive Hybi00_)
    ]

client :: Int -> (Iteratee ByteString IO () -> Iteratee ByteString IO a) -> IO a
client port app = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    hostAddr <- S.inet_addr "127.0.0.1"
    let addr = S.SockAddrInet (fromIntegral port) hostAddr
    S.connect sock addr
    res <- E.run_ $ receiveEnum sock $$ app $ sendIter sock
    S.sClose sock
    return res

webSocketsClient :: Protocol p => Int -> p -> WebSockets p a -> IO a
webSocketsClient port proto ws =
    client port $ runWebSocketsWith' defaultWebSocketsOptions proto ws

sendReceive :: forall p. TextProtocol p => p -> Property
sendReceive proto = monadicIO $ do
    run $ threadDelay (5 * 1000 * 1000)
    serverThread <- run $ forkIO $ runServer "0.0.0.0" 8000 server
    texts <- map unArbitraryUtf8 <$> pick arbitrary
    texts' <- run $ webSocketsClient 8000 proto $ client' texts
    run $ killThread serverThread
    assert $ texts == texts'
  where
    server :: Request -> WebSockets p ()
    server _ = forever $ do
        text <- receiveData
        sendTextData (text :: BL.ByteString)

    client' :: [BL.ByteString] -> WebSockets p [BL.ByteString]
    client' texts = do
        forM_ texts sendTextData
        replicateM (length texts) $ do
            receiveData
