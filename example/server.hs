{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Monad (forM_)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

type Client = (Text, WS.Sender Text)

-- | State kept on the server
type ServerState = [Client]

-- | Create a new, initial state
newServerState :: ServerState
newServerState = []

-- | Number of active clients
numClients :: ServerState -> Int
numClients = length

-- | Check if a user exists
clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

-- | Add a client
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

-- | Remove a client
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- | Send a message to all clients
sendMessage :: Text -> ServerState -> IO ()
sendMessage message clients = do
    T.putStrLn message
    forM_ clients $ \(_, sender) -> sender WS.textData message

-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ do
        -- Shake hands with the client, assume all is right
        Just rq <- WS.receiveRequest
        let Right rsp = WS.handshake rq
        WS.sendResponse rsp

        -- When a client succesfully connects, read his user ID and add
        -- him too the list
        mline <- WS.receiveData
        sender <- WS.getSender
        clients <- liftIO $ readMVar state
        case mline of
            Nothing -> return ()
            Just line
                | not (prefix `T.isPrefixOf` line) ->
                    WS.sendTextData ("Wrong announcement." :: Text)
                | clientExists client clients ->
                    WS.sendTextData ("User already exists." :: Text)
                | otherwise -> do
                    WS.sendTextData ("Welcome!" :: Text)
                    WS.sendTextData ("Welcome!" :: Text)
                    liftIO $ modifyMVar_ state $ \s -> do
                        let s' = addClient client s
                        sendMessage (fst client `mappend` " joined, " `mappend`
                            T.pack (show (numClients s')) `mappend`
                            " users connected") s'
                        return s'
                    talk state client
              where
                prefix = "Hi! I am "
                client = (T.drop (T.length prefix) line, sender)

-- Talks to the client (by echoing messages back) until EOF.
talk :: MVar ServerState -> Client -> WS.WebSockets ()
talk state client@(user, _) = do
    msg <- WS.receiveData
    case msg of
        Nothing -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = removeClient client s
            sendMessage (user `mappend` " disconnected") s'
            return s'
        Just m -> do
            liftIO $ readMVar state >>= sendMessage
                (user `mappend` ": " `mappend` m)
            talk state client
