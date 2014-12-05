--------------------------------------------------------------------------------
-- | This part of the library provides you with utilities to create WebSockets
-- clients (in addition to servers).
module Network.WebSockets.Client
    ( ClientApp
    , runClient
    , runClientWith
    , runClientWithSocket
    , runClientWithStream
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder      as Builder
import           Control.Concurrent.MVar       (newMVar)
import           Control.Exception             (finally, throwIO)
import           Data.IORef                    (newIORef)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Network.Socket                as S


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
runClientWith host port path opts customHeaders app = do
    -- Create and connect socket
    let hints = S.defaultHints
                    {S.addrFamily = S.AF_INET, S.addrSocketType = S.Stream}
    addrInfos <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock      <- S.socket S.AF_INET S.Stream S.defaultProtocol

    -- Connect WebSocket and run client
    res <- finally
        (S.connect sock (S.addrAddress $ head addrInfos) >>
         runClientWithSocket sock host path opts customHeaders app)
        (S.sClose sock)

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
    -- Create the request and send it
    request    <- createRequest protocol bHost bPath False customHeaders
    Stream.write stream (Builder.toLazyByteString $ encodeRequestHead request)
    mbResponse <- Stream.parse stream decodeResponseHead
    response   <- case mbResponse of
        Just response -> return response
        Nothing       -> throwIO $ OtherHandshakeException $
            "Network.WebSockets.Client.runClientWithStream: no handshake " ++
            "response from server"
    -- Note that we pattern match to evaluate the result here
    Response _ _ <- return $ finishResponse protocol request response
    parse        <- decodeMessages protocol stream
    write        <- encodeMessages protocol ClientConnection stream

    parseState <- newMVar (Available parse)
    writeState <- newMVar (Available write)
    sentRef    <- newIORef False
    app Connection
        { connectionOptions   = opts
        , connectionType      = ClientConnection
        , connectionProtocol  = protocol
        , connectionParse     = parseState
        , connectionWrite     = writeState
        , connectionSentClose = sentRef
        }
  where
    protocol = defaultProtocol  -- TODO
    bHost    = T.encodeUtf8 $ T.pack host
    bPath    = T.encodeUtf8 $ T.pack path


--------------------------------------------------------------------------------
runClientWithSocket :: S.Socket           -- ^ Socket
                    -> String             -- ^ Host
                    -> String             -- ^ Path
                    -> ConnectionOptions  -- ^ Options
                    -> Headers            -- ^ Custom headers to send
                    -> ClientApp a        -- ^ Client application
                    -> IO a
runClientWithSocket sock host path opts customHeaders app = do
    stream <- Stream.makeSocketStream sock
    runClientWithStream stream host path opts customHeaders app
