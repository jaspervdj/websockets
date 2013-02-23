--------------------------------------------------------------------------------
-- | This part of the library provides you with utilities to create WebSockets
-- clients (in addition to servers).
module Network.WebSockets.Client
    ( connect
    , connectWith
    , connectWithSocket
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative               ((<$>))
import           Control.Monad.Trans               (liftIO)
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as BC
import           Data.Enumerator                   (Iteratee, ($$))
import qualified Data.Enumerator                   as E
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Network.Socket                    as S
import qualified Network.Socket.Enumerator         as SE


--------------------------------------------------------------------------------
import           Network.WebSockets.Handshake.Http
import           Network.WebSockets.Monad
import           Network.WebSockets.Protocol
import           Network.WebSockets.Socket         (iterSocket)


--------------------------------------------------------------------------------
connect :: Protocol p
        => String          -- ^ Host
        -> Int             -- ^ Port
        -> String          -- ^ Path
        -> WebSockets p a  -- ^ Client application
        -> IO a
connect host port path ws =
    connectWith host port path Nothing Nothing ws


--------------------------------------------------------------------------------
connectWith :: Protocol p
            => String          -- ^ Host
            -> Int             -- ^ Port
            -> String          -- ^ Path
            -> Maybe String    -- ^ Origin, if Nothing then server interprets
                               --   connection as not coming from a browser.
            -> Maybe [String]  -- ^ Protocol List
            -> WebSockets p a  -- ^ Client application
            -> IO a
connectWith host port path origin wsProtocols app = do
    -- Create and connect socket
    let hints = S.defaultHints
                    {S.addrFamily = S.AF_INET, S.addrSocketType = S.Stream}
    sock      <- S.socket S.AF_INET S.Stream S.defaultProtocol
    addrInfos <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    S.connect sock (S.addrAddress $ head addrInfos)

    -- Connect WebSocket and run client
    res <- connectWithSocket sock host path origin wsProtocols app

    -- Clean up
    S.sClose sock
    return res


--------------------------------------------------------------------------------
connectWithSocket :: Protocol p
                  => S.Socket        -- ^ Socket
                  -> String          -- ^ Host
                  -> String          -- ^ Path
                  -> Maybe String    -- ^ Origin, if Nothing then server
                                     --   interprets connection as not coming
                                     --   from a browser.
                  -> Maybe [String]  -- ^ Protocol List
                  -> WebSockets p a  -- ^ Client application
                  -> IO a
connectWithSocket sock host path origin wsProtocols app = do
    -- Create the request
    request <- createRequest protocol bHost bPath bOrigin bWsProtocols False

    -- Connect to server
    E.run_ $ SE.enumSocket 4096 sock $$ (iter request) $ iterSocket sock
  where
    protocol      = head implementations
    iter request  = runWebSocketsClient protocol request app
    bHost         = T.encodeUtf8 $ T.pack host
    bPath         = T.encodeUtf8 $ T.pack path
    bOrigin       = T.encodeUtf8 . T.pack <$> origin
    bWsProtocols  = map BC.pack <$> wsProtocols


--------------------------------------------------------------------------------
runWebSocketsClient :: Protocol p
                    => p
                    -> RequestHttpPart
                    -> WebSockets p a
                    -> Iteratee ByteString IO ()
                    -> Iteratee ByteString IO a
runWebSocketsClient protocol request ws outIter = do
    liftIO $ makeBuilderSender outIter $ encodeRequestHttpPart request
    response <- receiveIteratee decodeResponse
    _        <- finishResponse protocol request response
    runWebSocketsWith' defaultWebSocketsOptions protocol ws outIter
