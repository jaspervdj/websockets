import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Network.Socket (setSocketOption, SocketOption (ReuseAddr))
import Data.ByteString.UTF8 (fromString) -- this is from utf8-string
import Control.Monad (forever, forM_, unless)
import Control.Concurrent (forkIO)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)

-- | State kept on the server
data ServerState = ServerState
    { nextClientId :: Int
    , clients :: [(Int, Handle)]
    }

-- | Create a new, initial state
newServerState :: ServerState
newServerState = ServerState 0 []

-- | Number of clients connected
numClients :: ServerState -> Int
numClients = length . clients

-- | Add a client and yield it's ID
addClient :: Handle -> ServerState -> (ServerState, Int)
addClient h (ServerState i c) = (ServerState (i + 1) ((i, h) : c), i)

-- | Remove a client by ID
removeClient :: Int -> ServerState -> ServerState
removeClient i state = state {clients = filter ((/= i) . fst) (clients state)}

-- | Send a message to clients except for the sender
sendMessage :: ByteString -> ServerState -> IO ()
sendMessage message state = do
    B.putStrLn message
    forM_ (clients state) $ \(client, handle) -> putFrame handle message

-- | Accepts clients, spawns a single handler for each one.
main :: IO ()
main = withSocketsDo $ do
    socket <- listenOn (PortNumber 8088)
    _ <- setSocketOption socket ReuseAddr 1
    putStrLn "Listening on port 8088."
    state <- newMVar newServerState
    forever $ do
        -- Wait for a new client to connect
        (h, _, _) <- accept socket

        -- Shake hands with the client
        request <- shakeHands h
        case request of
            Left err -> print err
            Right  _ -> do
                -- When a client succesfully connects, give him an ID and add
                -- him too the list
                i <- modifyMVar state $ return . addClient h

                -- Notification for others
                s <- readMVar state
                sendMessage (fromString $ "Client " ++ show i ++ " joined") s

                -- Communicate with the client in a separate thread
                _ <- forkIO (talk state i h)
                return ()

-- Talks to the client (by echoing messages back) until EOF.
talk :: MVar ServerState -> Int -> Handle -> IO ()
talk state client h = do
    msg <- getFrame h
    s <- readMVar state
    if B.null msg
        then do
            sendMessage
                (fromString $ "Client " ++ show client ++ " disconnected") s
            modifyMVar_ state $ return . removeClient client
            hClose h
        else do
            sendMessage
                (fromString ("Client " ++ show client ++ ": ") `B.append` msg) s
            talk state client h
