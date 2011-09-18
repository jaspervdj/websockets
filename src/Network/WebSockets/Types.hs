module Network.WebSockets.Types
    ( Headers
    , Request (..)
    , Response (..)
    , FrameType (..)
    , Frame (..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

-- | Request headers
type Headers = [(ByteString, ByteString)]

-- | Simple request type
data Request = Request
    { requestPath    :: !ByteString
    , requestHeaders :: Headers
    } deriving (Show)

-- | Response to a 'Request'
data Response = Response
    { responseHeaders :: Headers
    } deriving (Show)

-- | A frame
data Frame = Frame
    { frameFin     :: Bool
    , frameType    :: FrameType
    , framePayload :: BL.ByteString
    } deriving (Show)

-- | Type of a frame
data FrameType
    = Continuation
    | Text
    | Binary
    | Close
    | Ping
    | Pong
    deriving (Show)
