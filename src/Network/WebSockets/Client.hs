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
import           Control.Exception             (finally)
import qualified Data.ByteString               as B
import           Data.IORef                    (newIORef)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Network.Socket                as S
import qualified System.IO.Streams             as Streams
import qualified System.IO.Streams.Attoparsec  as Streams


--------------------------------------------------------------------------------
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Protocol
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
    :: (Streams.InputStream B.ByteString, Streams.OutputStream B.ByteString)
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
runClientWithStream (sIn, sOut) host path opts customHeaders app = do
    -- Create the request and send it
    request     <- createRequest protocol bHost bPath False customHeaders
    bOut        <- Streams.builderStream sOut
    Streams.write (Just $ encodeRequestHead request) bOut
    Streams.write (Just Builder.flush)               bOut
    response     <- Streams.parseFromStream decodeResponseHead sIn
    -- Note that we pattern match to evaluate the result here
    Response _ _ <- return $ finishResponse protocol request response
    mIn          <- decodeMessages protocol sIn
    mOut         <- encodeMessages protocol ClientConnection bOut
    sentRef      <- newIORef False
    app Connection
        { connectionOptions   = opts
        , connectionType      = ClientConnection
        , connectionProtocol  = protocol
        , connectionIn        = mIn
        , connectionOut       = mOut
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
    stream <- Streams.socketToStreams sock
    runClientWithStream stream host path opts customHeaders app
