-- | Wrapper for supporting multiple protocol versions
{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol
    ( Protocol (..)
    ) where

import Network.WebSockets.Decode (Decoder)
import Network.WebSockets.Encode (Encoder)
import Network.WebSockets.Types (Frame)

data Protocol = Protocol
  { version :: String
  , encodeFrame :: Encoder Frame
  , decodeFrame :: Decoder Frame
  }
