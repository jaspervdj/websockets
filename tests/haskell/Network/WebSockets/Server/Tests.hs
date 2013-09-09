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


--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy           as BL
import           System.Random                  (newStdGen)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?))
import           Test.QuickCheck                (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen            (Gen (..))


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Server.Tests"
    [ testCase "server/client" testServerClient
    ]


--------------------------------------------------------------------------------
sample :: Arbitrary a => IO [a]
sample = do
    gen <- newStdGen
    return $ (unGen arbitrary) gen 512


--------------------------------------------------------------------------------
testServerClient :: Assertion
testServerClient = do
    serverThread <- forkIO $ retry $ runServer "0.0.0.0" 42940 server
    waitSome
    texts  <- map unArbitraryUtf8 <$> sample
    texts' <- retry $ runClient "127.0.0.1" 42940 "/chat" $ client texts
    waitSome
    killThread serverThread
    texts @=? texts'
  where
    waitSome = threadDelay $ 200 * 1000

    server :: ServerApp
    server pc = do
        conn <- acceptRequest pc
        forever $ do
            text <- receiveData conn
            sendTextData conn (text :: BL.ByteString)

    client :: [BL.ByteString] -> ClientApp [BL.ByteString]
    client texts conn = do
        forM_ texts (sendTextData conn)
        texts' <- replicateM (length texts) (receiveData conn)
        sendClose conn ("Bye" :: BL.ByteString)
        return texts'

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
