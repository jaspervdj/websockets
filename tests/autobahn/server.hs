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
websockets-autobahn
-}

--------------------------------------------------------------------------------
import           Control.Exception          (catch)
import           Data.ByteString.Lazy.Char8 ()

--------------------------------------------------------------------------------
import qualified Network.WebSockets         as WS


--------------------------------------------------------------------------------
echoDataMessage :: WS.Connection -> IO ()
echoDataMessage conn = go 0
  where
    go :: Int -> IO ()
    go x = do
        msg <- WS.receiveDataMessage conn
        WS.sendDataMessage conn msg
        go (x + 1)


--------------------------------------------------------------------------------
-- | Application
application :: WS.ServerApp
application pc = do
    conn <-  WS.acceptRequest pc
    echoDataMessage conn `catch` handleClose

  where
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
    options = WS.defaultConnectionOptions
        { WS.connectionPermessageDeflate = Just WS.defaultPermessageDeflate
        }
