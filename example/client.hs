{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Monad.Trans (liftIO)
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS


app :: WS.WebSockets WS.Hybi10 ()
app = do
    liftIO $ print "Connected"

    -- Fork off a separate thread that reads from stdin and writes to the sink.
    sink <- WS.getSink
    liftIO $ forkIO $ readInput sink

    forever loop
  where
    loop = do
        msg <- WS.receiveData
        liftIO $ T.putStrLn msg

readInput :: (WS.TextProtocol p) => WS.Sink p -> IO ()
readInput sink = forever $ do
    line <- T.getLine
    WS.sendSink sink $ WS.textData line

main :: IO ()
main = do
    WS.connect "127.0.0.1" 9160 "/chat" app
