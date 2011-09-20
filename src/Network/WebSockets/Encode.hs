-- | Encoding of types to the WebSocket protocol. We always encode to 'Builder'
-- values.
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

import Data.Monoid (mappend, mempty, mconcat)

import Blaze.ByteString.Builder (Builder, fromLazyByteString)
import Blaze.ByteString.Builder.Word (fromWord8, fromWord16be, fromWord64be)
import Blaze.ByteString.Builder.ByteString (copyByteString)
import Data.Bits ((.|.))
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.WebSockets.Types

-- | The inverse of a parser
type Encoder a = a -> Builder

-- | Encode an HTTP upgrade response
response :: Encoder Response
response (Response headers) =
    copyByteString "HTTP/1.1 101 WebSocket Protocol Handshake\r\n" `mappend`
    mconcat (map header headers) `mappend` copyByteString "\r\n"
  where
    header (k, v) = mconcat $ map copyByteString [k, ": ", v, "\r\n"]

-- | Encode a frame
frame :: Encoder Frame
frame f = fromWord8 byte0 `mappend` fromWord8 byte1 `mappend` len `mappend`
    fromLazyByteString (framePayload f)
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

    byte1 = mask .|. lenflag
    mask  = 0x00  -- We don't support server masking for now
    len'  = BL.length (framePayload f)
    (lenflag, len)
        | len' < 126     = (fromIntegral len', mempty)
        | len' < 0x10000 = (126, fromWord16be (fromIntegral len'))
        | otherwise      = (127, fromWord64be (fromIntegral len'))

-- | Encode a message
message :: Encoder Message
message m = case m of
    ControlMessage m' -> controlMessage m' 
    DataMessage m'    -> dataMessage m'

-- | Encode a control message
controlMessage :: Encoder ControlMessage
controlMessage m = frame $ case m of
    Close pl -> Frame True CloseFrame pl
    Ping pl  -> Frame True PingFrame pl
    Pong pl  -> Frame True PongFrame pl

-- | Encode an application message
dataMessage :: Encoder DataMessage
dataMessage m = frame $ case m of
    Text pl   -> Frame True TextFrame pl
    Binary pl -> Frame True BinaryFrame pl

textData :: Encoder TL.Text
textData = dataMessage . Text . TL.encodeUtf8

binaryData :: Encoder BL.ByteString
binaryData = dataMessage . Binary
