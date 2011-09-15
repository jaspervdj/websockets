import Network (listenOn, PortID(PortNumber), withSocketsDo)
import Network.Socket (accept)
import Data.Text (Text)
import Network.Socket (setSocketOption, SocketOption (ReuseAddr))
import Control.Monad (forever, forM_)
import Control.Concurrent (forkIO)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.WebSockets

-- | State kept on the server
data ServerState = ServerState
    { nextClientId :: Int
    , clients :: [(Int, Sender Text)]
    }

-- | Create a new, initial state
newServerState :: ServerState
newServerState = ServerState 0 []

-- | Number of clients connected
numClients :: ServerState -> Int
numClients = length . clients

-- | Add a client and yield it's ID
addClient :: Sender Text -> ServerState -> (ServerState, Int)
addClient s (ServerState i c) = (ServerState (i + 1) ((i, s) : c), i)

-- | Remove a client by ID
removeClient :: Int -> ServerState -> ServerState
removeClient i state = state {clients = filter ((/= i) . fst) (clients state)}

-- | Send a message to clients except for the sender
sendMessage :: Text -> ServerState -> IO ()
sendMessage message state = do
    T.putStrLn message
    forM_ (clients state) $ \(_, sender) -> sender textData message

-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = withSocketsDo $ do
    socket <- listenOn (PortNumber 8088)
    _ <- setSocketOption socket ReuseAddr 1
    putStrLn "Listening on port 8088."
    state <- newMVar newServerState
    forever $ do
        -- Wait for a new client to connect
        (sock, _) <- accept socket

        _ <- forkIO $ runWithSocket sock $ do
            -- Shake hands with the client, assume all is right
            Just rq <- receiveRequest
            let Right rsp = handshake rq
            sendResponse rsp

            -- When a client succesfully connects, give him an ID and add
            -- him too the list
            sender <- getSender
            i <- liftIO $ modifyMVar state $ return . addClient sender

            -- Notification for others
            liftIO $ do
                s <- readMVar state
                sendMessage (T.pack $ "Client " ++ show i ++ " joined") s

            -- Communicate with the client in a separate thread
            talk state i
        return ()

-- Talks to the client (by echoing messages back) until EOF.
talk :: MVar ServerState -> Int -> WebSockets ()
talk state client = do
    msg <- receiveTextData
    case msg of
        Nothing -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = removeClient client s
            sendMessage
                (T.pack $ "Client " ++ show client ++ " disconnected") s'
            return s'
        Just m -> do
            liftIO $ readMVar state >>= sendMessage
                (T.pack ("Client " ++ show client ++ ": ") `T.append` m)
            talk state client
