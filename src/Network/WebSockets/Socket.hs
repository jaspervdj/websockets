-- | Simple module which provides the low-level socket handling in case you want
-- to write a stand-alone 'WebSockets' application.
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Socket
    ( runServer
    , runWithSocket
    , iterSocket
    ) where

import Prelude hiding (catch)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)

import Data.ByteString (ByteString)
import Data.Enumerator (Iteratee, ($$))
import Network.Socket (Socket)
import qualified Data.Enumerator as E
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Network.Socket.Enumerator as SE

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Monad
import Network.WebSockets.Protocol
import Network.WebSockets.Types

-- | Provides a simple server. This function blocks forever. Note that this
-- is merely provided for quick-and-dirty standalone applications, for real
-- applications, you should use a real server.
runServer :: Protocol p
          => String                        -- ^ Address to bind to
          -> Int                           -- ^ Port to listen on
          -> (Request -> WebSockets p ())  -- ^ Application to serve
          -> IO ()                         -- ^ Never returns
runServer host port ws = S.withSocketsDo $ do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    _ <- S.setSocketOption sock S.ReuseAddr 1
    host' <- S.inet_addr host
    S.bindSocket sock (S.SockAddrInet (fromIntegral port) host')
    S.listen sock 5
    flip catch (closeSock sock) $ forever $ do
        (conn, _) <- S.accept sock
        -- Voodoo fix: set this to True as soon as we notice the connection was
        -- closed. Will prevent iterSocket' from even trying to send anything.
        -- Without it, we got many "Couldn't decode text frame as UTF8" errors
        -- in the browser (although the payload is definitely UTF8).
        -- killRef <- newIORef False
        _ <- forkIO $ runWithSocket conn ws >> return ()
        return ()
  where
    closeSock :: Socket -> SomeException -> IO ()
    closeSock sock _ = S.sClose sock

-- | This function wraps 'runWebSockets' in order to provide a simple API for
-- stand-alone servers.
runWithSocket :: Protocol p
              => Socket -> (Request -> WebSockets p a) -> IO a
runWithSocket s ws = do
    r <- E.run $ SE.enumSocket 4096 s $$
        runWebSocketsWithHandshake defaultWebSocketsOptions False ws (iterSocket s)
    S.sClose s
    either (error . show) return r

-- | Create an iterator which writes to a socket. Throws a 'ConnectionClosed'
-- exception if the user attempts to write to a closed socket.
iterSocket :: Socket -> Iteratee ByteString IO ()
iterSocket s = E.continue go
  where
    go (E.Chunks []) = E.continue go
    go (E.Chunks cs) = do
        b <- liftIO $ S.sIsWritable s
        if b
            then E.tryIO (SB.sendMany s cs) >> E.continue go
            else E.throwError ConnectionClosed
    go E.EOF         = E.continue go
