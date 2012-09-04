-- | Wrapper for supporting multiple protocol versions
{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol
    ( Protocol (..)
    , TextProtocol
    , BinaryProtocol
    , close
    , ping
    , pong
    , textData
    , binaryData
    ) where

import Blaze.ByteString.Builder (Builder)
import qualified Data.ByteString as B
import qualified Data.Enumerator as E

import Network.WebSockets.Types
import Network.WebSockets.Handshake.Http
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe

class Protocol p where
    -- | Unique identifier for us.
    version         :: p -> String

    -- | Version accepted in the "Sec-WebSocket-Version " header. This is
    -- usually not the same, or derivable from "version", e.g. for hybi10, it's
    -- "7", "8" or "17".
    headerVersions  :: p -> [B.ByteString]

    -- | Determine if the protocol is compatible with a requested version. A
    -- default implementation exists which uses the @headerVersions@ of the
    -- protocol.
    supported       :: p -> RequestHttpPart -> Bool
    supported p h   = case getSecWebSocketVersion h of
        Just v -> v `elem` headerVersions p
        _      -> False

    -- | Encodes messages to binary 'Builder's. Takes a random source so it is
    -- able to do masking of frames (needed in some cases).
    encodeMessages  :: Monad m
                    => p
                    -> E.Enumeratee (Message p) Builder m a

    -- | Decodes messages from binary 'B.ByteString's.
    decodeMessages  :: Monad m => p -> E.Enumeratee B.ByteString (Message p) m a

    -- | Create a @Request@ that can be sent to the websockets server to open
    -- the connection.
    createRequest   :: p
                    -> B.ByteString         -- ^ Hostname of the server.
                    -> B.ByteString         -- ^ Path
                    -> Maybe B.ByteString   -- ^ Origin where we are connecting from.
                    -> Maybe [B.ByteString] -- ^ Protocols list.
                    -> Bool                 -- ^ Is the connection secure, i.e. wss.
                    -> IO RequestHttpPart   -- ^ HTTP request that can be sent to the
                                            --   to the server to initiate the connection.

    -- | Parse and validate the rest of the request. For hybi10, this is just
    -- validation, but hybi00 also needs to fetch a "security token"
    --
    -- In case of failure, this function may throw a 'HandshakeError'.
    -- be amended with the RequestHttpPart for the user)
    finishRequest   :: Monad m
                    => p -> RequestHttpPart
                    -> E.Iteratee B.ByteString m Request

    -- | Size of the challenge response body.
    responseSize :: p -> (Headers -> Int)

    -- | Parse and validate the handshake response received from the server.
    validateResponse :: Monad m
                     => p -> RequestHttpPart -> Response
                     -> E.Iteratee B.ByteString m ()

    -- | Validate handshake response from the websockets server.


    -- | Implementations of the specification
    implementations :: [p]

class Protocol p => TextProtocol p
class TextProtocol p => BinaryProtocol p

-- | Construct a close message
close :: (TextProtocol p, WebSocketsData a) => a -> Message p
close = Unsafe.close

-- | Construct a ping message
ping :: (BinaryProtocol p, WebSocketsData a) => a -> Message p
ping = Unsafe.ping

-- | Construct a pong message
pong :: (BinaryProtocol p, WebSocketsData a) => a -> Message p
pong = Unsafe.pong

-- | Construct a text message
textData :: (TextProtocol p, WebSocketsData a) => a -> Message p
textData = Unsafe.textData

-- | Construct a binary message
binaryData :: (BinaryProtocol p, WebSocketsData a) => a -> Message p
binaryData = Unsafe.binaryData
