-- | Encoding of types to the WebSocket protocol. We always encode to
-- 'B.Builder' values.
module Network.WebSockets.Encode {-# DEPRECATED "Encoder" #-}
    ( Encoder
    , message
    , controlMessage
    , dataMessage
    ) where

import Network.WebSockets.Types

-- | Encode a message
message :: Encoder p Frame -> Encoder p (Message p)
message frame mask msg = case msg of
    ControlMessage m -> controlMessage frame mask m
    DataMessage m    -> dataMessage frame mask m

-- | Encode a control message
controlMessage :: Encoder p Frame -> Encoder p (ControlMessage p)
controlMessage frame mask msg = frame mask $ case msg of
    Close pl -> Frame True CloseFrame pl
    Ping pl  -> Frame True PingFrame pl
    Pong pl  -> Frame True PongFrame pl

-- | Encode an application message
dataMessage :: Encoder p Frame -> Encoder p (DataMessage p)
dataMessage frame mask msg = frame mask $ case msg of
    Text pl   -> Frame True TextFrame pl
    Binary pl -> Frame True BinaryFrame pl
