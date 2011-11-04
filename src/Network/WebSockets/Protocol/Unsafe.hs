module Network.WebSockets.Protocol.Unsafe
    ( close
    , ping
    , pong
    , textData
    , binaryData
    ) where

import Network.WebSockets.Types

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
