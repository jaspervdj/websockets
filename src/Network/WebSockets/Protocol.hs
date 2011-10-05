-- | Wrapper for supporting multiple protocol versions
{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol
    (
      protocols
    ) where

import Network.WebSockets.Decode (Decoder)
import Network.WebSockets.Encode (Encoder)
import Network.WebSockets.Types (Frame, RequestHttpPart)

import Network.WebSockets.Types (Protocol(..))

import Network.WebSockets.Protocol.Hybi10 (hybi10)
import Network.WebSockets.Protocol.Hybi00 (hybi00)

protocols :: [Protocol]
protocols = [ hybi10
            , hybi00
            ]

