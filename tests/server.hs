-- | The server part of the tests
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 ()
import Data.Monoid (mappend)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.WebSockets

echo :: WebSockets ()
echo = do
    msg <- receiveTextData
    liftIO $ putStrLn $ show msg
    case msg of
        Just m -> send textData m >> echo
        _      -> return ()

ping :: WebSockets ()
ping = do
    forM_ ["Hai", "Come again?", "Right!"] $ \msg -> do
        send controlMessage $ Ping msg
        fr <- receiveMessage
        case fr of
            Just (ControlMessage (Pong msg'))
                | msg' == msg -> return ()
                | otherwise   -> error "wrong message from client"
            _ -> error "ping: client closed socket too soon"

    send dataMessage $ Text "OK"

closeMe :: WebSockets ()
closeMe = do
    msg <- receiveTextData
    case msg of
        Just "Close me!" -> return ()
        _                -> error "closeme: unexpected input"

concurrentSend :: WebSockets ()
concurrentSend = do
    sender <- getSender
    forM_ [1 :: Int .. 100] $ \i -> liftIO $ do
        _ <- forkIO $ sender textData $
            "Herp-a-derp " `mappend` TL.pack (show i)
        return ()

-- | All tests
tests :: [(ByteString, WebSockets ())]
tests =
    [ ("/echo", echo)
    , ("/ping", ping)
    , ("/close-me", closeMe)
    , ("/concurrent-send", concurrentSend)
    ]

-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = runServer "0.0.0.0" 8000 $ do
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
