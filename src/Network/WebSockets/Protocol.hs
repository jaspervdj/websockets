--------------------------------------------------------------------------------
-- | Wrapper for supporting multiple protocol versions
{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol
    ( Protocol (..)
    , protocols
    , compatible
    , finishRequest
    , finishResponse
    , encodeMessages
    , decodeMessages
    , createRequest
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString                   as B
import qualified System.IO.Streams                 as Streams


--------------------------------------------------------------------------------
import           Network.WebSockets.Handshake.Http
import qualified Network.WebSockets.Hybi10         as Hybi10
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
data Protocol
    = Hybi10
    deriving (Show)


--------------------------------------------------------------------------------
protocols :: [Protocol]
protocols = [Hybi10]


--------------------------------------------------------------------------------
compatible :: Protocol -> RequestHead -> Bool
compatible Hybi10 = Hybi10.compatible


--------------------------------------------------------------------------------
finishRequest :: Protocol -> RequestHead -> Response
finishRequest Hybi10 = Hybi10.finishRequest


--------------------------------------------------------------------------------
finishResponse :: Protocol -> RequestHead -> ResponseHead -> Response
finishResponse Hybi10 = Hybi10.finishResponse


--------------------------------------------------------------------------------
encodeMessages :: Protocol -> Streams.OutputStream B.ByteString
               -> IO (Streams.OutputStream Message)
encodeMessages Hybi10 = Hybi10.encodeMessages


--------------------------------------------------------------------------------
decodeMessages :: Protocol -> Streams.InputStream B.ByteString
               -> IO (Streams.InputStream Message)
decodeMessages Hybi10 = Hybi10.decodeMessages


--------------------------------------------------------------------------------
createRequest :: Protocol -> B.ByteString -> B.ByteString -> Maybe B.ByteString
              -> Maybe [B.ByteString] -> Bool
              -> IO RequestHead
createRequest Hybi10 = Hybi10.createRequest
