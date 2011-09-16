-- | This is the example included in the 'Network.WebSockets' documentation
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T

import Network.WebSockets

-- Accepts clients, spawns a single handler for each one.
main :: IO ()
main = runServer "0.0.0.0" 8000 $ do
    Just request <- receiveRequest
    case handshake request of
        Left err  -> liftIO $ print err
        Right rsp -> do
            sendResponse rsp
            sendTextData "Do you read me, Lieutenant Bowie?"
            liftIO $ putStrLn "Shook hands, sent welcome message."
            talkLoop

-- Talks to the client (by echoing messages back) until EOF.
talkLoop :: WebSockets ()
talkLoop = do
    msg <- receiveTextData
    case msg of
        Nothing -> liftIO $ putStrLn "EOF encountered, quitting"
        Just m  -> do
            sendTextData $ m `T.append` ", meow."
            talkLoop
