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
import qualified Network.WebSockets.Hybi13 as Hybi13
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
data Protocol
    = Hybi13
    deriving (Show)


--------------------------------------------------------------------------------
defaultProtocol :: Protocol
defaultProtocol = Hybi13


--------------------------------------------------------------------------------
protocols :: [Protocol]
protocols = [Hybi13]


--------------------------------------------------------------------------------
headerVersions :: Protocol -> [ByteString]
headerVersions Hybi13 = Hybi13.headerVersions


--------------------------------------------------------------------------------
compatible :: Protocol -> RequestHead -> Bool
compatible protocol req = case getRequestSecWebSocketVersion req of
    Just v -> v `elem` headerVersions protocol
    _      -> True  -- Whatever?


--------------------------------------------------------------------------------
finishRequest :: Protocol -> RequestHead -> Response
finishRequest Hybi13 = Hybi13.finishRequest


--------------------------------------------------------------------------------
finishResponse :: Protocol -> RequestHead -> ResponseHead -> Response
finishResponse Hybi13 = Hybi13.finishResponse


--------------------------------------------------------------------------------
encodeMessages :: Protocol -> ConnectionType
               -> Streams.OutputStream Builder
               -> IO (Streams.OutputStream Message)
encodeMessages Hybi13 = Hybi13.encodeMessages


--------------------------------------------------------------------------------
decodeMessages :: Protocol -> Streams.InputStream B.ByteString
               -> IO (Streams.InputStream Message)
decodeMessages Hybi13 = Hybi13.decodeMessages


--------------------------------------------------------------------------------
createRequest :: Protocol -> B.ByteString -> B.ByteString -> Maybe B.ByteString
              -> Maybe [B.ByteString] -> Bool
              -> IO RequestHead
createRequest Hybi13 = Hybi13.createRequest
