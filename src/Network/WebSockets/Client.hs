--------------------------------------------------------------------------------
-- | This part of the library provides you with utilities to create WebSockets
-- clients (in addition to servers).
module Network.WebSockets.Client
    ( connect
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString                   (ByteString)
import           Data.Enumerator                   (Iteratee, ($$))
import qualified Data.Enumerator                   as E
import qualified Network.Socket                    as S
import qualified Network.Socket.Enumerator         as SE


--------------------------------------------------------------------------------
import           Network.WebSockets.Handshake.Http
import           Network.WebSockets.Monad
import           Network.WebSockets.Protocol
import           Network.WebSockets.Socket         (iterSocket)


--------------------------------------------------------------------------------
connect :: Protocol p
        => String           -- ^ Host
        -> Int              -- ^ Port
        -> RequestHttpPart  -- ^ Path, headers etc.
        -> WebSockets p a   -- ^ Client application
        -> IO ()
connect host port request app = do
    sock     <- S.socket S.AF_INET S.Stream S.defaultProtocol
    hostAddr <- S.inet_addr host
    let addr = S.SockAddrInet (fromIntegral port) hostAddr
    S.connect sock addr
    res <- E.run_ $ SE.enumSocket 4096 sock $$ iter $ iterSocket sock
    S.sClose sock
    return res
  where
    iter = runWebSocketsClient request app


--------------------------------------------------------------------------------
runWebSocketsClient :: Protocol p
                    => RequestHttpPart
                    -> WebSockets p a
                    -> Iteratee ByteString IO ()
                    -> Iteratee ByteString IO ()
runWebSocketsClient request app = undefined
