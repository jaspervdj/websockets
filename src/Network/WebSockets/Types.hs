module Network.WebSockets.Types
    ( Headers
    , Request (..)
    , Response (..)
    , Frame
    ) where

import Data.ByteString (ByteString)

-- | Request headers
type Headers = [(ByteString, ByteString)]

-- | Simple request type
data Request = Request !ByteString Headers !ByteString
             deriving (Show)

-- | Response to a 'Request'
data Response = Response Headers !ByteString
              deriving (Show)

-- | An UTF-8 encoded frame
type Frame = ByteString
