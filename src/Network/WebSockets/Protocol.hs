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

import qualified Data.ByteString as B

import Network.WebSockets.Http
import Network.WebSockets.Types
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe

class Protocol p where
    version       :: p -> String        -- ^ Unique identifier for us.
    headerVersion :: p -> B.ByteString  -- ^ version as used in the "Sec-WebSocket-Version " header.
                                        -- this is usually not the same, or derivable from "version",
                                        -- e.g. for hybi10, it's "8".
    encodeFrame   :: p -> Encoder p Frame
    decodeFrame   :: p -> Decoder p Frame
    finishRequest :: p -> RequestHttpPart -> Decoder p (Either HandshakeError Request)
    -- ^ Parse and validate the rest of the request. For hybi10, this is just
    -- validation, but hybi00 also needs to fetch a "security token"
    --
    -- Todo: Maybe we should introduce our own simplified error type here. (to be
    -- amended with the RequestHttpPart for the user)

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
