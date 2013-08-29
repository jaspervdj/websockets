--------------------------------------------------------------------------------
-- | Wrapper for supporting multiple protocol versions
{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol
    ( Protocol (..)
    , defaultProtocol
    , protocols
    , compatible
    , headerVersions
    , finishRequest
    , finishResponse
    , encodeMessages
    , decodeMessages
    , createRequest
    ) where


--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder  (Builder)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import qualified System.IO.Streams         as Streams


--------------------------------------------------------------------------------
import           Network.WebSockets.Http
import qualified Network.WebSockets.Hybi10 as Hybi10
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
data Protocol
    = Hybi10
    deriving (Show)


--------------------------------------------------------------------------------
defaultProtocol :: Protocol
defaultProtocol = Hybi10


--------------------------------------------------------------------------------
protocols :: [Protocol]
protocols = [Hybi10]


--------------------------------------------------------------------------------
headerVersions :: Protocol -> [ByteString]
headerVersions Hybi10 = Hybi10.headerVersions


--------------------------------------------------------------------------------
compatible :: Protocol -> RequestHead -> Bool
compatible protocol req = case getRequestSecWebSocketVersion req of
    Just v -> v `elem` headerVersions protocol
    _      -> True  -- Whatever?


--------------------------------------------------------------------------------
finishRequest :: Protocol -> RequestHead -> Response
finishRequest Hybi10 = Hybi10.finishRequest


--------------------------------------------------------------------------------
finishResponse :: Protocol -> RequestHead -> ResponseHead -> Response
finishResponse Hybi10 = Hybi10.finishResponse


--------------------------------------------------------------------------------
encodeMessages :: Protocol -> Streams.OutputStream Builder
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
