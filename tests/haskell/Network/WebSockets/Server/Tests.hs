--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Server.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative            ((<$>), (<|>))
import           Control.Concurrent             (forkIO, killThread,
                                                 threadDelay)
import           Control.Exception              (SomeException, catch, handle)
import           Control.Monad                  (forever, replicateM, unless)
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)

--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy           as BL
import           Data.Text                      (Text)
import           System.Environment             (getEnvironment)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert, (@=?))
import           Test.QuickCheck                (Arbitrary, arbitrary)
import           Test.QuickCheck.Gen            (Gen (..))
import           Test.QuickCheck.Random         (newQCGen)


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Server.Tests"
    [ testCase "simple server/client" testSimpleServerClient
    , testCase "bulk server/client"   testBulkServerClient
    , testCase "onPong"               testOnPong
    , testCase "ipv6 server"          testIpv6Server
    ]


--------------------------------------------------------------------------------
testSimpleServerClient :: Assertion
testSimpleServerClient = testServerClient "127.0.0.1" $ \conn -> mapM_ (sendTextData conn)


--------------------------------------------------------------------------------
-- | This is a bit ugly but it seems CI services don't support ipv6 in 2018.
skipIpv6Incompatible :: Assertion -> Assertion
skipIpv6Incompatible assertion = do
    env <- getEnvironment
    case lookup "TRAVIS" env <|> lookup "CIRCLECI" env of
        Just "true" -> return ()
        _           -> assertion

--------------------------------------------------------------------------------
testIpv6Server :: Assertion
testIpv6Server = skipIpv6Incompatible $
    testServerClient "::1" $ \conn -> mapM_ (sendTextData conn)

--------------------------------------------------------------------------------
testBulkServerClient :: Assertion
testBulkServerClient = testServerClient "127.0.0.1" sendTextDatas

--------------------------------------------------------------------------------
testServerClient :: String -> (Connection -> [BL.ByteString] -> IO ()) -> Assertion
testServerClient host sendMessages = withEchoServer host 42940 "Bye" $ do
    texts  <- map unArbitraryUtf8 <$> sample
    texts' <- retry $ runClient host 42940 "/chat" $ client texts
    texts @=? texts'
  where
    client :: [BL.ByteString] -> ClientApp [BL.ByteString]
    client texts conn = do
        sendMessages conn texts
        texts' <- replicateM (length texts) (receiveData conn)
        sendClose conn ("Bye" :: BL.ByteString)
        expectCloseException conn "Bye"
        return texts'



--------------------------------------------------------------------------------
testOnPong :: Assertion
testOnPong = withEchoServer "127.0.0.1" 42941 "Bye" $ do
    gotPong <- newIORef False
    let opts = defaultConnectionOptions
                   { connectionOnPong = writeIORef gotPong True
                   }

    rcv <- runClientWith "127.0.0.1" 42941 "/" opts [] client
    assert rcv
    assert =<< readIORef gotPong
  where
    client :: ClientApp Bool
    client conn = do
        sendPing conn ("What's a fish without an eye?" :: Text)
        sendTextData conn ("A fsh!" :: Text)
        msg <- receiveData conn
        sendCloseCode conn 1000 ("Bye" :: BL.ByteString)
        expectCloseException conn "Bye"
        return $ "A fsh!" == (msg :: Text)


--------------------------------------------------------------------------------
sample :: Arbitrary a => IO [a]
sample = do
    gen <- newQCGen
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
withEchoServer :: String -> Int -> BL.ByteString -> IO a -> IO a
withEchoServer host port expectedClose action = do
    cRef <- newIORef False
    serverThread <- forkIO $ retry $ runServer host port (\c -> server c `catch` handleClose cRef)
    waitSome
    result <- action
    waitSome
    killThread serverThread
    closeCalled <- readIORef cRef
    unless closeCalled $ error "Expecting the CloseRequest exception"
    return result
  where
    server :: ServerApp
    server pc = do
        conn <- acceptRequest pc
        forever $ do
            msg <- receiveDataMessage conn
            sendDataMessage conn msg

    handleClose :: IORef Bool -> ConnectionException -> IO ()
    handleClose cRef (CloseRequest i msg) = do
        i @=? 1000
        msg @=? expectedClose
        writeIORef cRef True
    handleClose _ ConnectionClosed =
        error "Unexpected connection closed exception"
    handleClose _ (ParseException _) =
        error "Unexpected parse exception"
    handleClose _ (UnicodeException _) =
        error "Unexpected unicode exception"


--------------------------------------------------------------------------------
expectCloseException :: Connection -> BL.ByteString -> IO ()
expectCloseException conn msg = act `catch` handler
    where
        act = receiveDataMessage conn >> error "Expecting CloseRequest exception"
        handler (CloseRequest i msg') = do
            i @=? 1000
            msg' @=? msg
        handler ConnectionClosed = error "Unexpected connection closed"
        handler (ParseException _) = error "Unexpected parse exception"
        handler (UnicodeException _) = error "Unexpected unicode exception"
