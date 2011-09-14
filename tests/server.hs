-- | The server part of the tests
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Network (listenOn, PortID(PortNumber), withSocketsDo)
import Network.Socket (accept)
import Network.Socket (setSocketOption, SocketOption (ReuseAddr))

import Network.WebSockets

echo :: WebSockets ()
echo = do
    msg <- receiveFrame
    case msg of
        Just m  -> sendFrame m >> echo
        Nothing -> return ()

-- | All tests
tests :: [(ByteString, WebSockets ())]
tests = [("/echo", echo)]

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
            let Just test = lookup (requestPath rq) tests in test
        return ()
