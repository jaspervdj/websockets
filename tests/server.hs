-- | The server part of the tests
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Data.Monoid (mappend)
import Network (listenOn, PortID(PortNumber), withSocketsDo)
import Network.Socket (accept)
import Network.Socket (setSocketOption, SocketOption (ReuseAddr))
import qualified Data.ByteString.Char8 as BC

import Network.WebSockets

echo :: WebSockets ()
echo = receiveFrame >>= maybe (return ()) ((>> echo) . sendFrame)

closeMe :: WebSockets ()
closeMe = do
    msg <- receiveFrame
    case msg of
        Just "Close me!" -> return ()
        _ -> error "closeme: unexpected input"

concurrentSend :: WebSockets ()
concurrentSend = do
    sender <- getSender
    forM_ [1 :: Int .. 100] $ \i -> liftIO $ do
        _ <- forkIO $ sender frame $ "Herp-a-derp " `mappend` BC.pack (show i)
        return ()

-- | All tests
tests :: [(ByteString, WebSockets ())]
tests =
    [ ("/echo", echo)
    , ("/close-me", closeMe)
    , ("/concurrent-send", concurrentSend)
    ]

-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = withSocketsDo $ do
    socket <- listenOn (PortNumber 8000)
    _ <- setSocketOption socket ReuseAddr 1
    putStrLn "Listening on port 8000."
    forever $ do
        -- Wait for a new client to connect
        (sock, _) <- accept socket
        _ <- forkIO $ runWithSocket sock $ do
            -- Shake hands with the client, assume all is right
            Just rq <- receiveRequest
            let Right rsp = handshake rq
            sendResponse rsp

            -- When a client succesfully connects, lookup the requested test and
            -- run it
            let name = requestPath rq
            liftIO $ putStrLn $ "Starting test " ++ show name
            let Just test = lookup name tests in test
            liftIO $ putStrLn $ "Test " ++ show name ++ " finished"
        return ()
