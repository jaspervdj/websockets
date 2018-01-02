{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception        as E
import           Control.Monad            (forever)
import qualified Data.Text                as T
import qualified Network.WebSockets       as WS

showException :: String -> IO a -> IO a
showException area m = do
    errOrX <- E.try m
    case errOrX of
        Right x                  -> return x
        Left (E.SomeException e) -> do
            putStrLn $ "-> Caught exception in " ++ area ++ ": " ++ show e
            E.throwIO e

clientApp :: WS.ClientApp ()
clientApp = \connection -> do
    WS.sendTextData connection $ T.pack "Hello world!"
    forever $ do
        msg <- showException "receiveData" $ WS.receiveData connection
        putStrLn $ "Received message: " ++ T.unpack msg

main :: IO ()
main = do
    client <- Async.async $ WS.runClient "echo.websocket.org" 80 ""
        (\c -> showException "clientApp" (clientApp c))

    _canceller <- Async.async $ do
        threadDelay $ 3 * 1000 * 1000
        putStrLn "Cancelling client from canceller..."
        Async.cancel client

    putStrLn "Awaiting result from client..."
    result <- Async.waitCatch client
    putStrLn $ "Result: " ++ show result
