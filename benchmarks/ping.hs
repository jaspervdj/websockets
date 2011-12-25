{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forever)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Network.WebSockets as WS

ping :: WS.Request -> WS.WebSockets WS.Hybi00 ()
ping rq = do
    WS.acceptRequest rq
    WS.send $ WS.textData ("Ping 0" :: B.ByteString)
    forever $ do
        msg <- WS.receiveData
        let n = read (BC.unpack (B.drop 5 msg)) :: Int
        WS.send $ WS.textData $ BC.pack $ "Ping " ++ show (n + 1)

main :: IO ()
main = WS.runServer "0.0.0.0" 8088 ping
