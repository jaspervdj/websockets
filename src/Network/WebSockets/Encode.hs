-- | Encoding of types to the WebSocket protocol. We always encode to
-- 'B.Builder' values.
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Encode
    ( Encoder
    , response
    , frame
    , message
    , controlMessage
    , dataMessage
    , textData
    , binaryData
    ) where

import Data.Bits ((.|.))
import Data.Monoid (mappend, mempty, mconcat)

import Data.ByteString.Char8 ()
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Mask
import Network.WebSockets.Types

-- | The inverse of a parser
type Encoder a = Mask -> a -> B.Builder

-- | Encode an HTTP upgrade response
response :: Encoder Response
response _ (Response code msg headers) =
    B.copyByteString "HTTP/1.1 " `mappend` B.fromString (show code) `mappend`
    B.fromChar ' ' `mappend` B.fromByteString msg `mappend`
    B.fromByteString "\r\n" `mappend`
    mconcat (map header headers) `mappend` B.copyByteString "\r\n"
  where
    header (k, v) = mconcat $ map B.copyByteString
        [CI.original k, ": ", v, "\r\n"]

-- | Encode a frame
frame :: Encoder Frame
frame mask f = B.fromWord8 byte0 `mappend` B.fromWord8 byte1 `mappend`
    len `mappend` maskbytes `mappend`
    B.fromLazyByteString (maskPayload mask (framePayload f))
  where
    byte0  = fin .|. opcode
    fin    = if frameFin f then 0x80 else 0x00
    opcode = case frameType f of
        ContinuationFrame -> 0x00
        TextFrame         -> 0x01
        BinaryFrame       -> 0x02
        CloseFrame        -> 0x08
        PingFrame         -> 0x09
        PongFrame         -> 0x0a

    (maskflag, maskbytes) = case mask of
        Nothing -> (0x00, mempty)
        Just m  -> (0x80, B.fromByteString m)

    byte1 = maskflag .|. lenflag
    len'  = BL.length (framePayload f)
    (lenflag, len)
        | len' < 126     = (fromIntegral len', mempty)
        | len' < 0x10000 = (126, B.fromWord16be (fromIntegral len'))
        | otherwise      = (127, B.fromWord64be (fromIntegral len'))

-- | Encode a message
message :: Encoder Message
message mask msg = case msg of
    ControlMessage m -> controlMessage mask m
    DataMessage m    -> dataMessage mask m

-- | Encode a control message
controlMessage :: Encoder ControlMessage
controlMessage mask msg = frame mask $ case msg of
    Close pl -> Frame True CloseFrame pl
    Ping pl  -> Frame True PingFrame pl
    Pong pl  -> Frame True PongFrame pl

-- | Encode an application message
dataMessage :: Encoder DataMessage
dataMessage mask msg = frame mask $ case msg of
    Text pl   -> Frame True TextFrame pl
    Binary pl -> Frame True BinaryFrame pl

textData :: WebSocketsData a => Encoder a
textData mask = dataMessage mask . Text . toLazyByteString

binaryData :: WebSocketsData a => Encoder a
binaryData mask = dataMessage mask . Binary . toLazyByteString
