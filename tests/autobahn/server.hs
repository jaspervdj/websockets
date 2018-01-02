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
import           Data.String                (fromString)
import           Data.Version               (showVersion)


--------------------------------------------------------------------------------
import qualified Network.WebSockets         as WS
import qualified Paths_websockets


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
infoHeaders :: WS.Headers
infoHeaders =
    [ ( "Server"
      , fromString $ "websockets/" ++ showVersion Paths_websockets.version
      )
    ]


--------------------------------------------------------------------------------
-- | Application
application :: WS.ServerApp
application pc = do
    conn <-  WS.acceptRequestWith pc WS.defaultAcceptRequest
        { WS.acceptHeaders = infoHeaders
        }
    echoDataMessage conn `catch` handleClose

  where
    handleClose (WS.CloseRequest i "") =
        putStrLn $ "Clean close (" ++ show i ++ ")"
    handleClose (WS.CloseRequest i msg) =
        putStrLn $ "Clean close (" ++ show i ++ "): " ++ show msg
    handleClose WS.ConnectionClosed =
        putStrLn "Unexpected connection closed exception"
    handleClose (WS.ParseException e) =
        putStrLn $ "Recevied parse exception: " ++ show e
    handleClose (WS.UnicodeException e) =
        putStrLn $ "Recevied unicode exception: " ++ show e


--------------------------------------------------------------------------------
-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = WS.runServerWith "0.0.0.0" 9001 options application
  where
    options = WS.defaultConnectionOptions
        { WS.connectionCompressionOptions =
            WS.PermessageDeflateCompression WS.defaultPermessageDeflate
        , WS.connectionStrictUnicode      = True
        }
