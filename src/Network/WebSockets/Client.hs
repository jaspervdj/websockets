--------------------------------------------------------------------------------
-- | This part of the library provides you with utilities to create WebSockets
-- clients (in addition to servers).
module Network.WebSockets.Client
    ( ClientApp
    , runClient
    , runClientWith
    , runClientWithSocket
    , runClientWithStream
    , newClientConnection
    -- * Low level functionality
    , createRequest
    , Protocol(..)
    , defaultProtocol
    , checkServerResponse
    , streamToClientConnection
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString.Builder       as Builder
import           Control.Exception             (bracket, finally, throwIO)
import           Control.Concurrent.MVar       (newEmptyMVar)
import           Control.Monad                 (void)
import           Data.IORef                    (newIORef)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Network.Socket                as S
import           System.Timeout                (timeout)


--------------------------------------------------------------------------------
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Protocol
import           Network.WebSockets.Stream     (Stream)
import qualified Network.WebSockets.Stream     as Stream
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
-- | A client application interacting with a single server. Once this 'IO'
-- action finished, the underlying socket is closed automatically.
type ClientApp a = Connection -> IO a


--------------------------------------------------------------------------------
-- TODO: Maybe this should all be strings
runClient :: String       -- ^ Host
          -> Int          -- ^ Port
          -> String       -- ^ Path
          -> ClientApp a  -- ^ Client application
          -> IO a
runClient host port path ws =
    runClientWith host port path defaultConnectionOptions [] ws


--------------------------------------------------------------------------------
runClientWith :: String             -- ^ Host
              -> Int                -- ^ Port
              -> String             -- ^ Path
              -> ConnectionOptions  -- ^ Options
              -> Headers            -- ^ Custom headers to send
              -> ClientApp a        -- ^ Client application
              -> IO a
runClientWith host port path0 opts customHeaders app = do
    -- Create and connect socket
    let hints = S.defaultHints
                    {S.addrSocketType = S.Stream}

        -- Correct host and path.
        fullHost = if port == 80 then host else (host ++ ":" ++ show port)
        path     = if null path0 then "/" else path0
    addr:_ <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock      <- S.socket (S.addrFamily addr) S.Stream S.defaultProtocol
    S.setSocketOption sock S.NoDelay 1

    -- Connect WebSocket and run client
    res <- bracket
        (timeout (connectionTimeout opts * 1000 * 1000) $ S.connect sock (S.addrAddress addr))
        (const $ S.close sock) $ \maybeConnected -> case maybeConnected of
            Nothing -> throwIO $ ConnectionTimeout
            Just () -> runClientWithSocket sock fullHost path opts customHeaders app


    -- Clean up
    return res


--------------------------------------------------------------------------------

runClientWithStream
    :: Stream
    -- ^ Stream
    -> String
    -- ^ Host
    -> String
    -- ^ Path
    -> ConnectionOptions
    -- ^ Connection options
    -> Headers
    -- ^ Custom headers to send
    -> ClientApp a
    -- ^ Client application
    -> IO a
runClientWithStream stream host path opts customHeaders app = do
    newClientConnection stream host path opts customHeaders >>= app

-- | Build a new 'Connection' from the client's point of view.
--
-- /WARNING/: Be sure to call 'Stream.close' on the given 'Stream' after you are
-- done using the 'Connection' in order to properly close the communication
-- channel. 'runClientWithStream' handles this for you, prefer to use it when
-- possible.
newClientConnection
    :: Stream
    -- ^ Stream that will be used by the new 'Connection'.
    -> String
    -- ^ Host
    -> String
    -- ^ Path
    -> ConnectionOptions
    -- ^ Connection options
    -> Headers
    -- ^ Custom headers to send
    -> IO Connection
newClientConnection stream host path opts customHeaders = do
    -- Create the request and send it
    request    <- createRequest protocol bHost bPath False customHeaders
    Stream.write stream (Builder.toLazyByteString $ encodeRequestHead request)
    checkServerResponse stream request
    streamToClientConnection stream opts
  where
    protocol = defaultProtocol  -- TODO
    bHost    = T.encodeUtf8 $ T.pack host
    bPath    = T.encodeUtf8 $ T.pack path

-- | Check the response from the server.
-- Throws 'OtherHandshakeException' on failure
checkServerResponse :: Stream -> RequestHead -> IO ()
checkServerResponse stream request = do
    mbResponse <- Stream.parse stream decodeResponseHead
    response   <- case mbResponse of
        Just response -> return response
        Nothing       -> throwIO $ OtherHandshakeException $
            "Network.WebSockets.Client.newClientConnection: no handshake " ++
            "response from server"
    void $ either throwIO return $ finishResponse protocol request response
  where
    protocol = defaultProtocol -- TODO


-- | Build a 'Connection' from a pre-established stream with already finished
-- handshake.
--
-- /NB/: this will not perform any handshaking.
streamToClientConnection :: Stream -> ConnectionOptions -> IO Connection
streamToClientConnection stream opts = do
    parse   <- decodeMessages protocol
                (connectionFramePayloadSizeLimit opts)
                (connectionMessageDataSizeLimit opts) stream
    write   <- encodeMessages protocol ClientConnection stream
    sentRef <- newIORef False
    heartbeat <- newEmptyMVar
    return $ Connection
        { connectionOptions   = opts
        , connectionType      = ClientConnection
        , connectionProtocol  = protocol
        , connectionParse     = parse
        , connectionWrite     = write
        , connectionHeartbeat = heartbeat
        , connectionSentClose = sentRef
        }
  where
    protocol = defaultProtocol


--------------------------------------------------------------------------------
runClientWithSocket :: S.Socket           -- ^ Socket
                    -> String             -- ^ Host
                    -> String             -- ^ Path
                    -> ConnectionOptions  -- ^ Options
                    -> Headers            -- ^ Custom headers to send
                    -> ClientApp a        -- ^ Client application
                    -> IO a
runClientWithSocket sock host path opts customHeaders app = bracket
    (Stream.makeSocketStream sock)
    Stream.close
    (\stream ->
        runClientWithStream stream host path opts customHeaders app)
