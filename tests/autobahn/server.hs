--------------------------------------------------------------------------------
-- | The server part of the tests
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

{-

## once
virtualenv pyt
source pyt/bin/activate
### pip install --upgrade setuptools ### possibly
pip install autobahntestsuite

## each time
source pyt/bin/activate
mkdir -p test && cd test
wstest -m fuzzingclient

-}

--------------------------------------------------------------------------------
import           Control.Monad              (forever)
import           Control.Monad.Trans        (liftIO)
import           Control.Exception          (catch)
import           Data.ByteString.Lazy.Char8 ()


--------------------------------------------------------------------------------
import qualified Network.WebSockets         as WS


--------------------------------------------------------------------------------
echoDataMessage :: WS.Connection -> IO ()
echoDataMessage conn = forever $
    WS.sendDataMessage conn =<< WS.receiveDataMessage conn

--------------------------------------------------------------------------------
-- | Application
application :: WS.ServerApp
application pc = do
    conn <-  WS.acceptRequest pc
--     liftIO $ putStrLn $ "==================================="
--     liftIO $ putStrLn $ "Requested client version: " ++ show version'
--     liftIO $ putStrLn $ "Requested subprotocols: " ++ show (WS.getRequestSubprotocols rq)
--     liftIO $ putStrLn $ "Requested SecWebSocketExtensions: " ++ show (WS.getRequestSecWebSocketExtensions rq)
    echoDataMessage conn `catch` handleClose >> (liftIO $ putStrLn $ "Finished")

  where
    rq       = WS.pendingRequest pc
    version' = lookup "Sec-WebSocket-Version" (WS.requestHeaders rq)
    handleClose (WS.CloseRequest i msg) =
        putStrLn $ "Recevied close request " ++ show i ++ " : " ++ show msg
    handleClose WS.ConnectionClosed =
        putStrLn "Unexpected connection closed exception"
    handleClose (WS.ParseException e) =
        putStrLn $ "Recevied parse exception: " ++ show e


--------------------------------------------------------------------------------
-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = WS.runServerWith "0.0.0.0" 9001 options application
  where
    options = WS.defaultConnectionOptionsDeflate

