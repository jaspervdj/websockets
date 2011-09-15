{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Encode
    ( Encoder
    , response
    , builderData
    , byteStringData
    , textData
    ) where

import Data.Monoid (mappend, mconcat)

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Word (fromWord8)
import Blaze.ByteString.Builder.ByteString (copyByteString, fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Text (Text)

import Network.WebSockets.Types

type Encoder a = a -> Builder

response :: Encoder Response
response (Response headers token) =
    copyByteString "HTTP/1.1 101 WebSocket Protocol Handshake\r\n" `mappend`
    mconcat (map header headers) `mappend` copyByteString "\r\n" `mappend`
    copyByteString token
  where
    header (k, v) = mconcat $ map copyByteString [k, ": ", v, "\r\n"]

builderData :: Encoder Builder
builderData b = fromWord8 0 `mappend` b `mappend` fromWord8 0xff

byteStringData :: Encoder ByteString
byteStringData = builderData . fromByteString

textData :: Encoder Text
textData = builderData . fromText
