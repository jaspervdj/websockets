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

import Network.WebSockets.Types

-- | Encode an HTTP upgrade response
response :: Encoder p Response
response _ (Response code msg headers body) =
    B.copyByteString "HTTP/1.1 " `mappend` B.fromString (show code) `mappend`
    B.fromChar ' ' `mappend` B.fromByteString msg `mappend`
    B.fromByteString "\r\n" `mappend`
    mconcat (map header headers) `mappend` B.copyByteString "\r\n" `mappend`
    -- (body is empty except for version -00)
    B.copyByteString body
  where
    header (k, v) = mconcat $ map B.copyByteString
        [CI.original k, ": ", v, "\r\n"]

-- | Encode a message
message :: Encoder p Frame -> Encoder p Message
message frame mask msg = case msg of
    ControlMessage m -> controlMessage frame mask m
    DataMessage m    -> dataMessage frame mask m

-- | Encode a control message
controlMessage :: Encoder p Frame -> Encoder p ControlMessage
controlMessage frame mask msg = frame mask $ case msg of
    Close pl -> Frame True CloseFrame pl
    Ping pl  -> Frame True PingFrame pl
    Pong pl  -> Frame True PongFrame pl

-- | Encode an application message
dataMessage :: Encoder p Frame -> Encoder p DataMessage
dataMessage frame mask msg = frame mask $ case msg of
    Text pl   -> Frame True TextFrame pl
    Binary pl -> Frame True BinaryFrame pl
