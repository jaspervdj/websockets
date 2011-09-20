-- | The server part of the tests
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
import Control.Concurrent (forkIO)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 ()
import Data.Monoid (mappend)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.WebSockets

echo :: WebSockets ()
echo = do
    fr <- receiveFrame
    liftIO $ putStrLn $ show fr
    case fr of
        Just f -> sendFrame f >> echo
        _      -> return ()

ping :: WebSockets ()
ping = do
    forM_ ["Hai", "Come again?", "Right!"] $ \msg -> do
        sendFrame $ Frame True Ping msg
        fr <- receiveFrame
        case fr of
            Just (Frame True Pong msg')
                | msg' == msg -> return ()
                | otherwise   -> error "wrong message from client"
            _          -> error "ping: client closed socket too soon"

    sendFrame $ Frame True Text "OK"

closeMe :: WebSockets ()
closeMe = do
    msg <- receiveFrame
    case msg of
        Just (Frame _ _ "Close me!") -> return ()
        _                            -> error "closeme: unexpected input"

concurrentSend :: WebSockets ()
concurrentSend = do
    sender <- getSender
    forM_ [1 :: Int .. 100] $ \i -> liftIO $ do
        _ <- forkIO $ sender frame $ Frame True Text $
            TL.encodeUtf8 $ "Herp-a-derp " `mappend` TL.pack (show i)
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
