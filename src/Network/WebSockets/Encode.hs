-- | Encoding of types to the WebSocket protocol. We always encode to
-- 'B.Builder' values.
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Encode
    ( Encoder
    , response
    , message
    , controlMessage
    , dataMessage
    ) where

import Data.Monoid (mappend, mconcat)

import Data.ByteString.Char8 ()
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B
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

-- | Encode a message
message :: Encoder Frame -> Encoder Message
message frame mask msg = case msg of
    ControlMessage m -> controlMessage frame mask m
    DataMessage m    -> dataMessage frame mask m

-- | Encode a control message
controlMessage :: Encoder Frame -> Encoder ControlMessage
controlMessage frame mask msg = frame mask $ case msg of
    Close pl -> Frame True CloseFrame pl
    Ping pl  -> Frame True PingFrame pl
    Pong pl  -> Frame True PongFrame pl

-- | Encode an application message
dataMessage :: Encoder Frame -> Encoder DataMessage
dataMessage frame mask msg = frame mask $ case msg of
    Text pl   -> Frame True TextFrame pl
    Binary pl -> Frame True BinaryFrame pl
