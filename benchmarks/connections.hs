{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever, unless)
import Data.ByteString (ByteString)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Client as WC
import Criterion.Main
import Control.Concurrent.Async (mapConcurrently_, race_, async, wait, Async)


server :: WS.ServerApp
server pending = do
    conn <- WS.acceptRequest pending
    msg  <- WS.receiveData conn
    WS.sendBinaryData conn (msg :: ByteString)

client :: WC.ClientApp ()
client conn = do
    -- Send and receive a message back
    let msg = "Hello, world!" :: ByteString
    WS.sendBinaryData conn msg
    msg' <- WS.receiveData conn
    unless (msg == msg') $ error "Message mismatch"

    WS.sendClose conn ("Bye!" :: ByteString)

run :: Int -> Async () -> IO ()
run n server = race_ runServer runClient
    where 
    runClient = mapConcurrently_ (\_ -> WC.runClient "127.0.0.1" 8089 "/" client) [1..n]
    runServer = wait server

main :: IO ()
main = do
    server <- async $ WS.runServer "127.0.0.1" 8089 server
    defaultMain [
        bgroup "connections"
            [ bench "100" $ nfIO $ run 100 server
            , bench "1000" $ nfIO $ run 1000 server
            , bench "10000" $ nfIO $ run 10000 server
            , bench "100000" $ nfIO $ run 100000 server
            ]
        ]