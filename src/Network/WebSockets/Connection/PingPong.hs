module Network.WebSockets.Connection.PingPong
    ( withPingPong
    , PingPongOptions(..)
    , PongTimeout(..)
    , defaultPingPongOptions
    ) where 

import Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad (void)
import Network.WebSockets.Connection (Connection, connectionHeartbeat, pingThread)
import Control.Concurrent.MVar (takeMVar)
import System.Timeout (timeout)


-- | Exception type used to kill connections if there
-- is a pong timeout.
data PongTimeout = PongTimeout deriving Show

instance Exception PongTimeout


-- | Options for ping-pong
-- 
-- Make sure that the ping interval is less than the pong timeout,
-- for example N/2.
data PingPongOptions = PingPongOptions {
    pingInterval :: Int, -- ^ Interval in seconds
    pongTimeout :: Int, -- ^ Timeout in seconds
    pingAction :: IO () -- ^ Action to perform after sending a ping
}

-- | Default options for ping-pong
-- 
--   Ping every 15 seconds, timeout after 30 seconds
defaultPingPongOptions :: PingPongOptions
defaultPingPongOptions = PingPongOptions {
    pingInterval = 15,
    pongTimeout = 30,
    pingAction = return ()
}

-- | Run an application with ping-pong enabled. Raises PongTimeout if a pong is not received.
-- 
-- Can used with Client and Server connections.
withPingPong :: PingPongOptions -> Connection -> (Connection -> IO ()) -> IO ()
withPingPong options connection app = void $ 
    withAsync (app connection) $ \appAsync -> do
        withAsync (pingThread connection (pingInterval options) (pingAction options)) $ \pingAsync -> do
            withAsync (heartbeat >> throwIO PongTimeout) $ \heartbeatAsync -> do
                waitAnyCancel [appAsync, pingAsync, heartbeatAsync]
    where
        heartbeat = whileJust $ timeout (pongTimeout options * 1000 * 1000) 
           $ takeMVar (connectionHeartbeat connection)

        -- Loop until action returns Nothing
        whileJust :: IO (Maybe a) -> IO ()
        whileJust action = do
            result <- action
            case result of
                Nothing -> return ()
                Just _ -> whileJust action