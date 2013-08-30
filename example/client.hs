--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork off a separate thread that reads from stdin and writes to the WS
    _ <- forkIO $ forever $ do
        line <- T.getLine
        WS.sendTextData conn line

    -- In the main thread, keep reading from the WS and write to stdout
    forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg


--------------------------------------------------------------------------------
main :: IO ()
main = WS.runClient "echo.websocket.org" 80 "/" app
