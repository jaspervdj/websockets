--------------------------------------------------------------------------------
-- | This part of the library provides you with utilities to create WebSockets
-- clients (in addition to servers).
module Network.WebSockets.Client
    ( ClientApp
    , runClient
    , runClientWith
    , runClientWithSocket
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder      as Builder
import           Control.Applicative           ((<$>))
import           Control.Exception             (finally)
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Network.Socket                as S
import qualified System.IO.Streams             as Streams
import qualified System.IO.Streams.Attoparsec  as Streams


--------------------------------------------------------------------------------
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Protocol


--------------------------------------------------------------------------------
type ClientApp a = Connection -> IO a


--------------------------------------------------------------------------------
-- TODO: Maybe this should all be strings
runClient :: String       -- ^ Host
          -> Int          -- ^ Port
          -> String       -- ^ Path
          -> ClientApp a  -- ^ Client application
          -> IO a
runClient host port path ws =
    runClientWith host port path Nothing Nothing ws


--------------------------------------------------------------------------------
-- TODO: Maybe we should just allow the user to pass headers
runClientWith :: String          -- ^ Host
              -> Int             -- ^ Port
              -> String          -- ^ Path
              -> Maybe String    -- ^ Origin, if Nothing then server interprets
                                 --   connection as not coming from a browser.
              -> Maybe [String]  -- ^ Protocol List
              -> ClientApp a     -- ^ Client application
              -> IO a
runClientWith host port path origin wsProtocols app = do
    -- Create and connect socket
    let hints = S.defaultHints
                    {S.addrFamily = S.AF_INET, S.addrSocketType = S.Stream}
    sock      <- S.socket S.AF_INET S.Stream S.defaultProtocol
    addrInfos <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    S.connect sock (S.addrAddress $ head addrInfos)

    -- Connect WebSocket and run client
    res <- finally
        (runClientWithSocket sock host path origin wsProtocols app)
        (S.sClose sock)

    -- Clean up
    return res


--------------------------------------------------------------------------------
runClientWithSocket :: S.Socket        -- ^ Socket
                    -> String          -- ^ Host
                    -> String          -- ^ Path
                    -> Maybe String    -- ^ Origin, if Nothing then server
                                       --   interprets connection as not coming
                                       --   from a browser.
                    -> Maybe [String]  -- ^ Protocol List
                    -> ClientApp a     -- ^ Client application
                    -> IO a
runClientWithSocket sock host path origin wsProtocols app = do
    -- Create the request and send it
    request     <- createRequest protocol bHost bPath bOrigin bWsProtocols False
    (sIn, sOut) <- Streams.socketToStreams sock
    bOut        <- Streams.builderStream sOut
    Streams.write (Just $ encodeRequestHead request) bOut
    Streams.write (Just Builder.flush)               bOut
    response     <- Streams.parseFromStream decodeResponseHead sIn
    -- Note that we pattern match to evaluate the result here
    Response _ _ <- return $ finishResponse protocol request response
    mIn          <- decodeMessages protocol sIn
    mOut         <- encodeMessages protocol bOut
    app Connection
        { connectionProtocol = protocol
        , connectionIn       = mIn
        , connectionOut      = mOut
        }
  where
    protocol      = defaultProtocol  -- TODO
    bHost         = T.encodeUtf8 $ T.pack host
    bPath         = T.encodeUtf8 $ T.pack path
    bOrigin       = T.encodeUtf8 . T.pack <$> origin
    bWsProtocols  = map BC.pack <$> wsProtocols
