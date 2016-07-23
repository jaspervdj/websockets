--------------------------------------------------------------------------------
-- | This provides a simple stand-alone server for 'WebSockets' applications.
-- Note that in production you want to use a real webserver such as snap or
-- warp.
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Server
    ( ServerApp
    , runServer
    , runServerWith
    , makeListenSocket
    , makePendingConnection
    , makePendingConnectionFromStream
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent            (forkIOWithUnmask)
import           Control.Exception             (allowInterrupt, bracket,
                                                bracketOnError, finally, mask_,
                                                throwIO)
import           Control.Monad                 (forever, void)
import           Network.Socket                (Socket)
import qualified Network.Socket                as S


--------------------------------------------------------------------------------
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import qualified Network.WebSockets.Stream     as Stream
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
-- | WebSockets application that can be ran by a server. Once this 'IO' action
-- finishes, the underlying socket is closed automatically.
type ServerApp = PendingConnection -> IO ()


--------------------------------------------------------------------------------
-- | Provides a simple server. This function blocks forever. Note that this
-- is merely provided for quick-and-dirty standalone applications, for real
-- applications, you should use a real server.
--
-- Glue for using this package with real servers is provided by:
--
-- * <https://hackage.haskell.org/package/wai-websockets>
--
-- * <https://hackage.haskell.org/package/websockets-snap>
runServer :: String     -- ^ Address to bind
          -> Int        -- ^ Port to listen on
          -> ServerApp  -- ^ Application
          -> IO ()      -- ^ Never returns
runServer host port app = runServerWith host port defaultConnectionOptions app


--------------------------------------------------------------------------------
-- | A version of 'runServer' which allows you to customize some options.
runServerWith :: String -> Int -> ConnectionOptions -> ServerApp -> IO ()
runServerWith host port opts app = S.withSocketsDo $
  bracket
  (makeListenSocket host port)
  S.sClose
  (\sock ->
    mask_ $ forever $ do
      allowInterrupt
      (conn, _) <- S.accept sock
      void $ forkIOWithUnmask $ \unmask ->
        finally (unmask $ runApp conn opts app) (S.sClose conn)
    )



--------------------------------------------------------------------------------
-- | Create a standardized socket on which you can listen for incomming
-- connections. Should only be used for a quick and dirty solution! Should be
-- preceded by the call 'Network.Socket.withSocketsDo'.
makeListenSocket :: String -> Int -> IO Socket
makeListenSocket host port = bracketOnError
    (S.socket S.AF_INET S.Stream S.defaultProtocol)
    S.sClose
    (\sock -> do
        _     <- S.setSocketOption sock S.ReuseAddr 1
        _     <- S.setSocketOption sock S.NoDelay   1
        host' <- S.inet_addr host
        S.bindSocket sock (S.SockAddrInet (fromIntegral port) host')
        S.listen sock 5
        return sock
        )


--------------------------------------------------------------------------------
runApp :: Socket
       -> ConnectionOptions
       -> ServerApp
       -> IO ()
runApp socket opts app =
    bracket
        (makePendingConnection socket opts)
        (Stream.close . pendingStream)
        app


--------------------------------------------------------------------------------
-- | Turns a socket, connected to some client, into a 'PendingConnection'. The
-- 'PendingConnection' should be closed using 'Stream.close' later.
makePendingConnection
    :: Socket -> ConnectionOptions -> IO PendingConnection
makePendingConnection socket opts = do
    stream <- Stream.makeSocketStream socket
    makePendingConnectionFromStream stream opts


-- | More general version of 'makePendingConnection' for 'Stream.Stream'
-- instead of a 'Socket'.
makePendingConnectionFromStream
    :: Stream.Stream -> ConnectionOptions -> IO PendingConnection
makePendingConnectionFromStream stream opts = do
    -- TODO: we probably want to send a 40x if the request is bad?
    mbRequest <- Stream.parse stream (decodeRequestHead False)
    case mbRequest of
        Nothing      -> throwIO ConnectionClosed
        Just request -> return PendingConnection
            { pendingOptions  = opts
            , pendingRequest  = request
            , pendingOnAccept = \_ -> return ()
            , pendingStream   = stream
            }
