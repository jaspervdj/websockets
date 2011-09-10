import Network (listenOn, PortID(PortNumber), withSocketsDo)
import Network.Socket (accept)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.Socket (setSocketOption, SocketOption (ReuseAddr))
import Data.ByteString.UTF8 (fromString) -- this is from utf8-string
import Control.Monad (forever, forM_)
import Control.Concurrent (forkIO)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)

import Network.WebSockets

-- | State kept on the server
data ServerState = ServerState
    { nextClientId :: Int
    , clients :: [(Int, WebSocket)]
    }

-- | Create a new, initial state
newServerState :: ServerState
newServerState = ServerState 0 []

-- | Number of clients connected
numClients :: ServerState -> Int
numClients = length . clients

-- | Add a client and yield it's ID
addClient :: WebSocket -> ServerState -> (ServerState, Int)
addClient ws (ServerState i c) = (ServerState (i + 1) ((i, ws) : c), i)

-- | Remove a client by ID
removeClient :: Int -> ServerState -> ServerState
removeClient i state = state {clients = filter ((/= i) . fst) (clients state)}

-- | Send a message to clients except for the sender
sendMessage :: ByteString -> ServerState -> IO ()
sendMessage message state = do
    B.putStrLn message
    forM_ (clients state) $ \(_, ws) -> sendFrame ws message

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
        ws <- new sock

        -- Shake hands with the client
        Just request <- receiveRequest ws
        case handshake request of
            Left err -> print err
            Right response -> do
                sendResponse ws response

                -- When a client succesfully connects, give him an ID and add
                -- him too the list
                i <- modifyMVar state $ return . addClient ws

                -- Notification for others
                s <- readMVar state
                sendMessage (fromString $ "Client " ++ show i ++ " joined") s

                -- Communicate with the client in a separate thread
                _ <- forkIO (talk state i ws)
                return ()

-- Talks to the client (by echoing messages back) until EOF.
talk :: MVar ServerState -> Int -> WebSocket -> IO ()
talk state client ws = do
    msg <- receiveFrame ws
    s <- readMVar state
    case msg of
        Nothing -> do
            sendMessage
                (fromString $ "Client " ++ show client ++ " disconnected") s
            modifyMVar_ state $ return . removeClient client
        Just m -> do
            sendMessage
                (fromString ("Client " ++ show client ++ ": ") `B.append` m) s
            talk state client ws
