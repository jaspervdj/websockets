--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Server.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                         ((<$>))
import           Control.Concurrent                          (forkIO,
                                                              killThread,
                                                              threadDelay)
import           Control.Exception                           (SomeException,
                                                              handle)
import           Control.Monad                               (forM_, forever,
                                                              replicateM)


--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy                        as BL
import           System.Random                               (newStdGen)
import           Test.Framework                              (Test, testGroup)
import           Test.Framework.Providers.HUnit              (testCase)
import           Test.HUnit                                  (Assertion, (@=?))
import           Test.QuickCheck                             (Arbitrary,
                                                              arbitrary)
import           Test.QuickCheck.Gen                         (Gen (..))


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Protocol.Hybi10.Internal
import           Network.WebSockets.Tests.Util
import           Network.WebSockets.Tests.Util.Http


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Server.Tests"
    [ testCase "sendReceive-hybi10" (sendReceive Hybi10)
    -- TODO: Write client calls for Hybi00?
    -- , testCase "sendReceive-hybi00" (sendReceive Hybi00_)
    ]


--------------------------------------------------------------------------------
sample :: Arbitrary a => IO [a]
sample = do
    gen <- newStdGen
    return $ (unGen arbitrary) gen 512


--------------------------------------------------------------------------------
sendReceive :: Protocol -> Assertion
sendReceive protocol = do
    serverThread <- forkIO $ retry $ runServer "0.0.0.0" 42940 server
    waitSome
    texts  <- map unArbitraryUtf8 <$> sample
    texts' <- retry $ connect "127.0.0.1" 42940 "/chat" $ client' texts
    waitSome
    killThread serverThread
    texts @=? texts'
  where
    waitSome = threadDelay $ 200 * 1000

    server :: Request -> WebSockets p ()
    server rq = flip catchWsError (\_ -> return ()) $ do
        acceptRequest rq
        forever $ do
            text <- receiveData
            sendTextData (text :: BL.ByteString)

    client' :: [BL.ByteString] -> WebSockets p [BL.ByteString]
    client' texts = do
        -- sendBuilder $ encodeRequestBody $ exampleRequest proto
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
