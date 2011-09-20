module Network.WebSockets.Types
    ( Headers
    , Request (..)
    , Response (..)
    , FrameType (..)
    , Frame (..)
    , Message (..)
    , ControlMessage (..)
    , DataMessage (..)
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
    = ContinuationFrame
    | TextFrame
    | BinaryFrame
    | CloseFrame
    | PingFrame
    | PongFrame
    deriving (Show)

-- | The kind of message a server application typically deals with
data Message
    = ControlMessage ControlMessage
    | DataMessage DataMessage
    deriving (Show)

-- | Different control messages
data ControlMessage
    = Close BL.ByteString
    | Ping BL.ByteString
    | Pong BL.ByteString
    deriving (Show)

-- | For an end-user of this library, dealing with 'Frame's would be a bit
-- low-level. This is why define another type on top of it, which represents
-- data for the application layer.
data DataMessage
    = Text BL.ByteString
    | Binary BL.ByteString
    deriving (Show)
