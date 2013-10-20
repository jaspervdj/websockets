--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Server.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>))
import           Control.Concurrent             (forkIO, killThread,
                                                 threadDelay)
import           Control.Exception              (SomeException, handle)
import           Control.Monad                  (forM_, forever, replicateM)
import           Data.IORef                     (newIORef, readIORef,
                                                 writeIORef)


--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy           as BL
import           Data.Text                      (Text)
import           System.Random                  (newStdGen)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert, (@=?))
import           Test.QuickCheck                (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen            (Gen (..))


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Server.Tests"
    [ testCase "simple server/client" testSimpleServerClient
    , testCase "onPong"               testOnPong
    ]


--------------------------------------------------------------------------------
testSimpleServerClient :: Assertion
testSimpleServerClient = withEchoServer 42940 $ do
    texts  <- map unArbitraryUtf8 <$> sample
    texts' <- retry $ runClient "127.0.0.1" 42940 "/chat" $ client texts
    texts @=? texts'
  where
    client :: [BL.ByteString] -> ClientApp [BL.ByteString]
    client texts conn = do
        forM_ texts (sendTextData conn)
        texts' <- replicateM (length texts) (receiveData conn)
        sendClose conn ("Bye" :: BL.ByteString)
        return texts'


--------------------------------------------------------------------------------
testOnPong :: Assertion
testOnPong = withEchoServer 42941 $ do
    gotPong <- newIORef False
    let opts = defaultConnectionOptions
                   { connectionOnPong = writeIORef gotPong True
                   }

    rcv <- runClientWith "127.0.0.1" 42941 "/" opts Nothing Nothing client
    assert rcv
    assert =<< readIORef gotPong
  where
    client :: ClientApp Bool
    client conn = do
        sendPing conn ("What's a fish without an eye?" :: Text)
        sendTextData conn ("A fsh!" :: Text)
        msg <- receiveData conn
        return $ "A fsh!" == (msg :: Text)


--------------------------------------------------------------------------------
sample :: Arbitrary a => IO [a]
sample = do
    gen <- newStdGen
    return $ (unGen arbitrary) gen 512


--------------------------------------------------------------------------------
waitSome :: IO ()
waitSome = threadDelay $ 200 * 1000


--------------------------------------------------------------------------------
-- HOLY SHIT WHAT SORT OF ATROCITY IS THIS?!?!?!
--
-- The problem is that sometimes, the server hasn't been brought down yet
-- before the next test, which will cause it not to be able to bind to the
-- same port again. In this case, we just retry.
--
-- The same is true for our client: possibly, the server is not up yet
-- before we run the client. We also want to retry in that case.
retry :: IO a -> IO a
retry action = (\(_ :: SomeException) -> waitSome >> action) `handle` action


--------------------------------------------------------------------------------
withEchoServer :: Int -> IO a -> IO a
withEchoServer port action = do
    serverThread <- forkIO $ retry $ runServer "0.0.0.0" port server
    waitSome
    result <- action
    waitSome
    killThread serverThread
    return result
  where
    server :: ServerApp
    server pc = do
        conn <- acceptRequest pc
        forever $ do
            msg <- receiveDataMessage conn
            sendDataMessage conn msg
