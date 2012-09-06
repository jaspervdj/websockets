--------------------------------------------------------------------------------
-- | This part of the library provides you with utilities to create WebSockets
-- clients (in addition to servers).
module Network.WebSockets.Client
    ( connect
    , connectWith
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                     (liftM)
import           Control.Monad.Trans               (liftIO)
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Char8             (pack)
import           Data.Enumerator                   (Iteratee, ($$))
import qualified Data.Enumerator                   as E
import qualified Network.Socket                    as S
import qualified Network.Socket.Enumerator         as SE


--------------------------------------------------------------------------------
import           Network.WebSockets.Handshake.Http
import           Network.WebSockets.Monad
import           Network.WebSockets.Protocol
import           Network.WebSockets.Socket         (iterSocket)
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
connect :: Protocol p
        => String          -- ^ Host
        -> Int             -- ^ Port
        -> String          -- ^ Path
        -> Bool            -- ^ Secure
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
            -> Bool            -- ^ Secure
            -> WebSockets p a  -- ^ Client application
            -> IO a
connectWith host port path origin wsProtocols secure app = do
    -- Create the request
    request <- createRequest protocol bHost bPath bOrigin bWsProtocols secure

    -- Connect to server
    sock     <- S.socket S.AF_INET S.Stream S.defaultProtocol
    hostAddr <- S.inet_addr host
    let addr = S.SockAddrInet (fromIntegral port) hostAddr
    S.connect sock addr
    res <- E.run_ $ SE.enumSocket 4096 sock $$ (iter request) $ iterSocket sock

    -- Clean up
    S.sClose sock
    return res
  where
    protocol      = head implementations
    iter request  = runWebSocketsClient protocol request app
    bHost         = pack host
    bPath         = pack path
    bOrigin       = pack `liftM` origin
    bWsProtocols  = (map pack) `liftM` wsProtocols


--------------------------------------------------------------------------------
runWebSocketsClient :: Protocol p
                    => p
                    -> RequestHttpPart
                    -> WebSockets p a
                    -> Iteratee ByteString IO ()
                    -> Iteratee ByteString IO a
runWebSocketsClient protocol request ws outIter = do
    liftIO $ makeBuilderSender outIter $ encodeRequestHttpPart request
    response <- receiveIteratee $ decodeResponse (responseSize protocol)
    validateResponse protocol request response
    runWebSocketsWith' defaultWebSocketsOptions protocol ws outIter
