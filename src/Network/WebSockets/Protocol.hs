-- | Wrapper for supporting multiple protocol versions
{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol
    ( Protocol (..)
    ) where

import qualified Data.ByteString as B

import Network.WebSockets.Decode (Decoder)
import Network.WebSockets.Encode (Encoder)
import Network.WebSockets.Types

class Protocol p where
    version       :: p -> String        -- ^ Unique identifier for us.
    headerVersion :: p -> B.ByteString  -- ^ version as used in the "Sec-WebSocket-Version " header.
                                        -- this is usually not the same, or derivable from "version",
                                        -- e.g. for hybi10, it's "8".
    encodeFrame   :: p -> Encoder Frame
    decodeFrame   :: p -> Decoder Frame
    finishRequest :: p -> RequestHttpPart -> Decoder (Either HandshakeError Request)
    -- ^ Parse and validate the rest of the request. For hybi10, this is just
    -- validation, but hybi00 also needs to fetch a "security token"
    --
    -- Todo: Maybe we should introduce our own simplified error type here. (to be
    -- amended with the RequestHttpPart for the user)

    -- | Implementations of the specification
    implementations :: [p]
