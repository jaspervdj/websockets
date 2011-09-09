{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Encode
    ( Encoder
    , response
    , frame
    ) where

import Data.Monoid (mappend, mconcat)

import Data.ByteString.Char8 ()
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Word (fromWord8)
import Blaze.ByteString.Builder.ByteString (copyByteString)

import Network.WebSockets.Types

type Encoder a = a -> Builder

response :: Encoder Response
response (Response headers token) =
    copyByteString "HTTP/1.1 101 WebSocket Protocol Handshake\r\n" `mappend`
    mconcat (map header headers) `mappend` copyByteString "\r\n" `mappend`
    copyByteString token
  where
    header (k, v) = mconcat $ map copyByteString [k, ": ", v, "\r\n"]

frame :: Encoder Frame
frame bs = fromWord8 0 `mappend` copyByteString bs `mappend` fromWord8 0xff
