-- | Wrapper for supporting multiple protocol versions
{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol
    ( Protocol (..)
    , SomeProtocol (..)
    ) where

import Network.WebSockets.Decode (Decoder)
import Network.WebSockets.Encode (Encoder)
import Network.WebSockets.Types (Frame)

class Protocol p where
    encodeFrame :: p -> Encoder Frame
    decodeFrame :: p -> Decoder Frame

data SomeProtocol = forall p. Protocol p => SomeProtocol p

instance Protocol SomeProtocol where
    encodeFrame (SomeProtocol p) = encodeFrame p
    decodeFrame (SomeProtocol p) = decodeFrame p
