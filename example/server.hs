import Network (listenOn, PortID(PortNumber), withSocketsDo)
import Network.Socket (accept)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.Socket (setSocketOption, SocketOption (ReuseAddr))
import Data.ByteString.UTF8 (fromString) -- this is from utf8-string
import Control.Monad (forever, forM_)
import Control.Concurrent (forkIO)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)

import Network.WebSockets

-- | State kept on the server
data ServerState = ServerState
    { nextClientId :: Int
    , clients :: [(Int, Sender Frame)]
    }

-- | Create a new, initial state
newServerState :: ServerState
newServerState = ServerState 0 []

-- | Number of clients connected
numClients :: ServerState -> Int
numClients = length . clients

-- | Add a client and yield it's ID
addClient :: Sender Frame -> ServerState -> (ServerState, Int)
addClient s (ServerState i c) = (ServerState (i + 1) ((i, s) : c), i)

-- | Remove a client by ID
removeClient :: Int -> ServerState -> ServerState
removeClient i state = state {clients = filter ((/= i) . fst) (clients state)}

-- | Send a message to clients except for the sender
sendMessage :: ByteString -> ServerState -> IO ()
sendMessage message state = do
    B.putStrLn message
    forM_ (clients state) $ \(_, sender) -> sender frame message

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
                sendMessage (fromString $ "Client " ++ show i ++ " joined") s

            -- Communicate with the client in a separate thread
            talk state i
        return ()

-- Talks to the client (by echoing messages back) until EOF.
talk :: MVar ServerState -> Int -> WebSockets ()
talk state client = do
    msg <- receiveFrame
    case msg of
        Nothing -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = removeClient client s
            sendMessage
                (fromString $ "Client " ++ show client ++ " disconnected") s'
            return s'
        Just m -> do
            liftIO $ readMVar state >>= sendMessage
                (fromString ("Client " ++ show client ++ ": ") `B.append` m)
            talk state client
