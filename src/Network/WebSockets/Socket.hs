-- | Simple module which provides the low-level socket handling in case you want
-- to write a stand-alone 'WebSockets' application.
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Socket
    ( runServer
    , runWithSocket
    ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)

import Network.Socket ( Family (..), SockAddr (..), Socket
                      , SocketOption (ReuseAddr), SocketType (..)
                      , accept, bindSocket, defaultProtocol, inet_addr, listen
                      , sClose, setSocketOption, socket, withSocketsDo
                      )
import Network.Socket.ByteString (recv, sendMany)

import Data.ByteString (ByteString)
import Data.Enumerator ( Enumerator, Iteratee (..), Stream (..)
                       , checkContinue0, continue, yield, (>>==)
                       )

import Network.WebSockets.Monad

-- | Provides a simple server. This function blocks forever. Note that this
-- is merely provided for quick-and-dirty standalone applications, for real
-- applications, you should use a real server.
runServer :: String         -- ^ Address to bind to
          -> Int            -- ^ Port to listen on
          -> WebSockets ()  -- ^ Application to serve
          -> IO ()          -- ^ Never returns
runServer host port ws = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    _ <- setSocketOption sock ReuseAddr 1
    host' <- inet_addr host
    bindSocket sock (SockAddrInet (fromIntegral port) host')
    listen sock 5
    forever $ do
        (conn, _) <- accept sock
        _ <- forkIO $ runWithSocket conn ws
        return ()

-- | This function wraps 'runWebSockets' in order to provide a simple API for
-- stand-alone servers.
runWithSocket :: Socket -> WebSockets a -> IO a
runWithSocket s ws = do
    r <- runWebSockets ws (receiveEnum s) (sendIter s)
    sClose s
    either (error . show) return r

receiveEnum :: Socket -> Enumerator ByteString IO a
receiveEnum s = checkContinue0 $ \loop f -> do
    b <- liftIO $ recv s 4096
    if b == ""
        then continue f
        else f (Chunks [b]) >>== loop

sendIter :: Socket -> Iteratee ByteString IO ()
sendIter s = continue go
  where
    go (Chunks []) = continue go
    go (Chunks cs) = liftIO (sendMany s cs) >> continue go
    go EOF         = yield () EOF
