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
data Request = Request
    { requestPath    :: !ByteString
    , requestHeaders :: Headers
    , requestToken   :: !ByteString
    } deriving (Show)

-- | Response to a 'Request'
data Response = Response
    { responseHeaders :: Headers
    , responseToken   :: !ByteString
    } deriving (Show)

-- | An UTF-8 encoded frame
type Frame = ByteString
