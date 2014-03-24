{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad      (forever)
import qualified Network.WebSockets as WS

echo :: WS.Connection -> IO ()
echo conn = forever $ do
    msg <- WS.receiveDataMessage conn
    WS.sendDataMessage conn msg

main :: IO ()
main = WS.runServer "0.0.0.0" 9160 $ \pending -> do
    conn <- WS.acceptRequest pending
    echo conn
