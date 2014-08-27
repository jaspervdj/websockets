--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where


--------------------------------------------------------------------------------
import Control.Monad (forever)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.WebSockets as WS


--------------------------------------------------------------------------------
ping :: WS.ServerApp
ping pending = do
    conn <- WS.acceptRequest pending
    WS.sendTextData conn ("Ping 0" :: B.ByteString)
    forever $ do
        msg <- WS.receiveData conn
        let n = read (BC.unpack (B.drop 5 msg)) :: Int
        WS.sendTextData conn $ BC.pack $ "Ping " ++ show (n + 1)


--------------------------------------------------------------------------------
main :: IO ()
main = WS.runServer "0.0.0.0" 8088 ping
