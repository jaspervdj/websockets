{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Socket.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad (forever, forM_, replicateM)

import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee, ($$))
import System.Random (newStdGen)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen (..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator as E
import qualified Network.Socket as S
import qualified Network.Socket.Enumerator as SE

import Network.WebSockets
import Network.WebSockets.Handshake.Http
import Network.WebSockets.Monad
import Network.WebSockets.Socket
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Tests.Util
import Network.WebSockets.Tests.Util.Http

tests :: Test
tests = testGroup "Network.WebSockets.Socket.Tests"
    [ testCase "sendReceive-hybi10" (sendReceive Hybi10_)
    , testCase "sendReceive-hybi00" (sendReceive Hybi00_)
    ]

client :: Int -> (Iteratee ByteString IO () -> Iteratee ByteString IO a) -> IO a
client port app = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    hostAddr <- S.inet_addr "127.0.0.1"
    let addr = S.SockAddrInet (fromIntegral port) hostAddr
    S.connect sock addr
    res <- E.run_ $ SE.enumSocket 4096 sock $$ app $ iterSocket sock
    S.sClose sock
    return res

webSocketsClient :: Protocol p => Int -> p -> WebSockets p a -> IO a
webSocketsClient port proto ws =
    client port $ runWebSocketsWith' defaultWebSocketsOptions proto ws

sample :: Arbitrary a => IO [a]
sample = do
    gen <- newStdGen
    return $ (unGen arbitrary) gen 512

sendReceive :: forall p. (ExampleRequest p, TextProtocol p) => p -> Assertion
sendReceive proto = do
    serverThread <- forkIO $ retry $ runServer "0.0.0.0" 42940 server
    waitSome
    texts  <- map unArbitraryUtf8 <$> sample
    texts' <- retry $ webSocketsClient 42940 proto $ client' texts
    waitSome
    killThread serverThread
    texts @=? texts'
  where
    waitSome = threadDelay $ 200 * 1000

    server :: Request -> WebSockets p ()
    server _ = flip catchWsError (\_ -> return ()) $ forever $ do
        text <- receiveData
        sendTextData (text :: BL.ByteString)

    client' :: [BL.ByteString] -> WebSockets p [BL.ByteString]
    client' texts = do
        sendBuilder $ encodeRequestBody $ exampleRequest proto
        forM_ texts sendTextData
        replicateM (length texts) $ do
            receiveData

    -- HOLY SHIT WHAT SORT OF ATROCITY IS THIS?!?!?!
    --
    -- The problem is that sometimes, the server hasn't been brought down yet
    -- before the next test, which will cause it not to be able to bind to the
    -- same port again. In this case, we just retry.
    --
    -- The same is true for our client: possibly, the server is not up yet
    -- before we run the client. We also want to retry in that case.
    retry :: IO a -> IO a
    retry action = (\(_ :: SomeException) -> action) `handle` action
