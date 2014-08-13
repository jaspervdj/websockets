--------------------------------------------------------------------------------
-- | The server part of the tests
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Monad              (forM_, forever, void)
import           Control.Monad.Trans        (liftIO)
import           Control.Exception          (catch)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy.Char8 ()
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL


--------------------------------------------------------------------------------
import qualified Network.WebSockets         as WS


--------------------------------------------------------------------------------
echoText :: WS.Connection -> IO ()
echoText conn = forever $ do
    msg <- WS.receiveData conn
    liftIO $ putStrLn $ show (msg :: TL.Text)
    WS.sendTextData conn msg


--------------------------------------------------------------------------------
closeMe :: WS.Connection -> IO ()
closeMe conn = do
    msg <- WS.receiveData conn
    case (msg :: TL.Text) of
        "Close me!" -> do
            WS.sendClose conn ("Closing" :: ByteString)
            void $ WS.receiveDataMessage conn
            error "Expecting receiveDataMessage to throw CloseRequest exception"
        _           -> error "closeme: unexpected input"


--------------------------------------------------------------------------------
ping :: WS.Connection -> IO ()
ping conn = do
    forM_ ["Hai", "Come again?", "Right!"] $ \msg -> do
        WS.send conn $ WS.ControlMessage $ WS.Ping msg
        rsp <- WS.receive conn
        case rsp of
            WS.ControlMessage (WS.Pong msg')
                | msg' == msg -> return ()
                | otherwise   -> error "wrong message from client"
            _ -> error "ping: client closed socket too soon"

    WS.sendTextData conn ("OK" :: Text)


--------------------------------------------------------------------------------
echo :: WS.Connection -> IO ()
echo conn = forever $ WS.receive conn >>= WS.send conn


--------------------------------------------------------------------------------
tests :: [(ByteString, WS.Connection -> IO ())]
tests =
    [ ("/echo-text",   echoText)
    , ("/close-me",    closeMe)
    , ("/ping",        ping)
    , ("/echo",        echo)
    , ("/subprotocol", echoText)
    ]


--------------------------------------------------------------------------------
-- | Application
application :: WS.ServerApp
application pc = do
    let name = WS.requestPath rq
    -- When a client succesfully connects, lookup the requested test and
    -- run it
    conn <- case name of
        "/subprotocol" -> WS.acceptRequestWith pc $ WS.AcceptRequest $ Just "abc"
        _ -> WS.acceptRequest pc
    -- version'' <- WS.getVersion
    liftIO $ putStrLn $ "==================================="
    liftIO $ putStrLn $ "Requested client version: " ++ show version'
    -- liftIO $ putStrLn $ "Selected version: " ++ version''
    liftIO $ putStrLn $ "Requested subprotocols: " ++ show (WS.pendingSubprotocols pc)
    liftIO $ putStrLn $ "Starting test " ++ show name
    let Just test = lookup name tests in test conn `catch` handleClose
    liftIO $ putStrLn $ "Test " ++ show name ++ " finished"
  where
    rq       = WS.pendingRequest pc
    version' = lookup "Sec-WebSocket-Version" (WS.requestHeaders rq)
    handleClose (WS.CloseRequest i msg) =
        putStrLn $ "Recevied close request " ++ show i ++ " : " ++ show msg
    handleClose WS.ConnectionClosed =
        putStrLn "Unexpected connection closed exception"


--------------------------------------------------------------------------------
-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = WS.runServer "0.0.0.0" 8000 application
