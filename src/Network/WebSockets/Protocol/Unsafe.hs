module Network.WebSockets.Protocol.Unsafe
    ( castMessage
    , close
    , ping
    , pong
    , textData
    , binaryData
    ) where

import Network.WebSockets.Types

castMessage :: Message p1 -> Message p2
castMessage (ControlMessage m) = ControlMessage $ case m of
    Close b -> Close b
    Ping b  -> Ping b
    Pong b  -> Pong b
castMessage (DataMessage m)    = DataMessage $ case m of
    Text b   -> Text b
    Binary b -> Binary b
{-# INLINE castMessage #-}

close :: WebSocketsData a => a -> Message p
close = ControlMessage . Close . toLazyByteString

ping :: WebSocketsData a => a -> Message p
ping = ControlMessage . Ping . toLazyByteString

pong :: WebSocketsData a => a -> Message p
pong = ControlMessage . Pong . toLazyByteString

textData :: WebSocketsData a => a -> Message p
textData = DataMessage . Text . toLazyByteString

binaryData :: WebSocketsData a => a -> Message p
binaryData = DataMessage . Binary . toLazyByteString
