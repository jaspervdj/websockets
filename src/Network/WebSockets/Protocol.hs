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
import qualified Network.WebSockets.Hybi00 as Hybi00
import qualified Network.WebSockets.Hybi13 as Hybi13
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
data Protocol
    = Hybi13
    | Hybi00
    deriving (Show)


--------------------------------------------------------------------------------
defaultProtocol :: Protocol
defaultProtocol = Hybi13


--------------------------------------------------------------------------------
protocols :: [Protocol]
protocols = [Hybi13, Hybi00]


--------------------------------------------------------------------------------
headerVersions :: Protocol -> [ByteString]
headerVersions Hybi13 = Hybi13.headerVersions
headerVersions Hybi00 = Hybi00.headerVersions


--------------------------------------------------------------------------------
compatible :: Protocol -> RequestHead -> Bool
compatible protocol req = case getRequestSecWebSocketVersion req of
    Just v -> v `elem` headerVersions protocol
    _      -> True  -- Whatever?


--------------------------------------------------------------------------------
-- | The 'finishRequest' function gets access to the underlying inputstream. The
-- request head has already been parsed at this point, but the body is not. This
-- is only used for older version of the protocol (Hybi00)
finishRequest :: Protocol -> RequestHead -> Streams.InputStream B.ByteString
              -> IO Response
finishRequest Hybi13 reqHead _  = return $ Hybi13.finishRequest reqHead
finishRequest Hybi00 reqHead is = Hybi00.finishRequest reqHead is


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
createRequest :: Protocol -> B.ByteString -> B.ByteString -> Bool -> Headers
              -> IO RequestHead
createRequest Hybi13 = Hybi13.createRequest
