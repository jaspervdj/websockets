-- | The server part of the tests
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 ()
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

import qualified Network.WebSockets as WS

echo :: WS.WebSockets ()
echo = do
    msg <- WS.receiveData
    liftIO $ putStrLn $ show (msg :: Maybe TL.Text)
    case msg of
        Just m -> WS.sendTextData m >> echo
        _      -> return ()

ping :: WS.WebSockets ()
ping = do
    forM_ ["Hai", "Come again?", "Right!"] $ \msg -> do
        WS.sendMessage $ WS.ping msg
        fr <- WS.receiveMessage
        case fr of
            Just (WS.ControlMessage (WS.Pong msg'))
                | msg' == msg -> return ()
                | otherwise   -> error "wrong message from client"
            _ -> error "ping: client closed socket too soon"

    WS.sendMessage $ WS.textData ("OK" :: Text)

closeMe :: WS.WebSockets ()
closeMe = do
    msg <- WS.receiveData
    case (msg :: Maybe TL.Text) of
        Just "Close me!" -> return ()
        _                -> error "closeme: unexpected input"

concurrentSend :: WS.WebSockets ()
concurrentSend = do
    sender <- WS.getMessageSender
    liftIO $ do
        mvars <- mapM newMVar [1 :: Int .. 100]
        forM_ mvars $ \mvar -> forkIO $ do
            i <- readMVar mvar
            sender $ WS.textData $ "Herp-a-derp " `mappend` TL.pack (show i)
            _ <- takeMVar mvar
            return ()
        forM_ mvars $ flip putMVar 0

-- | All tests
tests :: [(ByteString, WS.WebSockets ())]
tests =
    [ ("/echo", echo)
    , ("/ping", ping)
    , ("/close-me", closeMe)
    , ("/concurrent-send", concurrentSend)
    ]

-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = WS.runServer "0.0.0.0" 8000 $ do
    -- Shake hands with the client, assume all is right
    Just rq <- WS.receiveRequest
    let Right rsp = WS.handshake rq
    WS.sendResponse rsp

    -- When a client succesfully connects, lookup the requested test and
    -- run it
    let name = WS.requestPath rq
    liftIO $ putStrLn $ "Starting test " ++ show name
    let Just test = lookup name tests in test
    liftIO $ putStrLn $ "Test " ++ show name ++ " finished"
