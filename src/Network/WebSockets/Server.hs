--------------------------------------------------------------------------------
-- | This provides a simple stand-alone server for 'WebSockets' applications.
-- Note that in production you want to use a real webserver such as snap or
-- warp.
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Server
    ( ServerApp
    , runServer
    , ServerOptions (..)
    , defaultServerOptions
    , runServerWithOptions
    , runServerWith
    , makeListenSocket
    , makePendingConnection
    , makePendingConnectionFromStream

    , PongTimeout
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent            (threadDelay)
import qualified Control.Concurrent.Async      as Async
import           Control.Exception             (Exception, allowInterrupt,
                                                bracket, bracketOnError,
                                                finally, mask_, throwIO)
import           Control.Monad                 (forever, void, when)
import qualified Data.IORef                    as IORef
import           Data.Maybe                    (isJust)
import           Network.Socket                (Socket)
import qualified Network.Socket                as S
import qualified System.Clock                  as Clock


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
-- | Provides a simple server. This function blocks forever.  Note that this
-- is merely provided for quick-and-dirty or internal applications, but for real
-- applications, you should use a real server.
--
-- For example:
--
-- * Performance is reasonable under load, but:
-- * No protection against DoS attacks is provided.
-- * No logging is performed.
-- * ...
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
runServerWith host port opts = runServerWithOptions defaultServerOptions
    { serverHost              = host
    , serverPort              = port
    , serverConnectionOptions = opts
    }
{-# DEPRECATED runServerWith "Use 'runServerWithOptions' instead" #-}


--------------------------------------------------------------------------------
data ServerOptions = ServerOptions
    { serverHost              :: String
    , serverPort              :: Int
    , serverConnectionOptions :: ConnectionOptions
    -- | Require a pong from the client every N seconds; otherwise kill the
    -- connection.  If you use this, you should also use 'withPingThread' to
    -- send a ping at a smaller interval; for example N/2.
    , serverRequirePong       :: Maybe Int
    }


--------------------------------------------------------------------------------
defaultServerOptions :: ServerOptions
defaultServerOptions = ServerOptions
    { serverHost              = "127.0.0.1"
    , serverPort              = 8080
    , serverConnectionOptions = defaultConnectionOptions
    , serverRequirePong       = Nothing
    }


--------------------------------------------------------------------------------
-- | Customizable version of 'runServer'.  Never returns until killed.
--
-- Please use the 'defaultServerOptions' combined with record updates to set the
-- fields you want.  This way your code is unlikely to break on future changes.
runServerWithOptions :: ServerOptions -> ServerApp -> IO a
runServerWithOptions opts app = S.withSocketsDo $
    bracket
    (makeListenSocket host port)
    S.close $ \sock -> mask_ $ forever $ do
        allowInterrupt
        (conn, _) <- S.accept sock

        -- This IORef holds a time at which the thread may be killed.  This time
        -- can be extended by calling 'tickle'.
        killRef <- IORef.newIORef =<< (+ killDelay) <$> getSecs
        let tickle = IORef.writeIORef killRef =<< (+ killDelay) <$> getSecs

        -- Update the connection options to call 'tickle' whenever a pong is
        -- received.
        let connOpts'
                | not useKiller = connOpts
                | otherwise     = connOpts
                    { connectionOnPong = tickle >> connectionOnPong connOpts
                    }

        -- Run the application.
        appAsync  <- Async.asyncWithUnmask $ \unmask ->
            (unmask $ do
                runApp conn connOpts' app) `finally`
            (S.close conn)

        -- Install the killer if required.
        when useKiller $ void $ Async.async (killer killRef appAsync)
  where
    host     = serverHost opts
    port     = serverPort opts
    connOpts = serverConnectionOptions opts

    -- Get the current number of seconds on some clock.
    getSecs = Clock.sec <$> Clock.getTime Clock.Monotonic

    -- Parse the 'serverRequirePong' options.
    useKiller = isJust $ serverRequirePong opts
    killDelay = maybe 0 fromIntegral (serverRequirePong opts)

    -- Thread that reads the killRef, and kills the application if enough time
    -- has passed.
    killer killRef appAsync = do
        killAt   <- IORef.readIORef killRef
        now      <- getSecs
        appState <- Async.poll appAsync
        case appState of
            -- Already finished/killed/crashed, we can give up.
            Just _ -> return ()
            -- Should not be killed yet.  Wait and try again.
            Nothing | now < killAt -> do
                threadDelay (fromIntegral killDelay * 1000 * 1000)
                killer killRef appAsync
            -- Time to kill.
            _ -> Async.cancelWith appAsync PongTimeout


--------------------------------------------------------------------------------
-- | Create a standardized socket on which you can listen for incomming
-- connections. Should only be used for a quick and dirty solution! Should be
-- preceded by the call 'Network.Socket.withSocketsDo'.
makeListenSocket :: String -> Int -> IO Socket
makeListenSocket host port = do
  addr:_ <- S.getAddrInfo (Just hints) (Just host) (Just (show port))
  bracketOnError
    (S.socket (S.addrFamily addr) S.Stream S.defaultProtocol)
    S.close
    (\sock -> do
        _     <- S.setSocketOption sock S.ReuseAddr 1
        _     <- S.setSocketOption sock S.NoDelay   1
        S.bind sock (S.addrAddress addr)
        S.listen sock 5
        return sock
        )
  where
    hints = S.defaultHints { S.addrSocketType = S.Stream }


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
    stream <- case connectionTlsSettings opts of
      Nothing -> Stream.makeSocketStream socket
      Just tls -> Stream.makeTlsSocketStream tls socket
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


--------------------------------------------------------------------------------
-- | Internally used exception type used to kill connections if there
-- is a pong timeout.
data PongTimeout = PongTimeout deriving Show


--------------------------------------------------------------------------------
instance Exception PongTimeout
