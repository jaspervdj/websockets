module Network.WebSockets.Types
    ( Request (..)
    , Frame
    ) where

import Data.ByteString (ByteString)

-- | Simple request type
data Request = Request !ByteString [(ByteString, ByteString)] !ByteString
             deriving (Show)

-- | An UTF-8 encoded frame
type Frame = ByteString
