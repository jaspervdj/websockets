-- | HTTP utilities
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Tests.Util.Http
    ( ExampleRequest (..)
    , RequestBody (..)
    , parseResponse
    , encodeRequestBody
    ) where

import Data.Monoid (mappend, mconcat)
import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Data.Attoparsec (Parser)
import Data.ByteString.Internal (c2w)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Types
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal

class ExampleRequest p where
    exampleRequest :: p -> RequestBody

data RequestBody = RequestBody RequestHttpPart B.ByteString
    deriving (Show)

instance ExampleRequest Hybi00_ where
    exampleRequest _ = RequestBody
        ( RequestHttpPart "/demo"
          [ ("Host", "example.com")
          , ("Connection", "Upgrade")
          , ("Sec-WebSocket-Key2", "12998 5 Y3 1  .P00")
          , ("Sec-WebSocket-Protocol", "sample")
          , ("Upgrade", "WebSocket")
          , ("Sec-WebSocket-Key1", "4 @1  46546xW%0l 1 5")
          , ("Origin", "http://example.com")
          ]
        )
        "^n:ds[4U"

instance ExampleRequest Hybi10_ where
    exampleRequest _ = RequestBody
        ( RequestHttpPart "/chat"
          [ ("Host", "server.example.com")
          , ("Upgrade", "websocket")
          , ("Connection", "Upgrade")
          , ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
          , ("Sec-WebSocket-Origin", "http://example.com")
          , ("Sec-WebSocket-Protocol", "chat, superchat")
          , ("Sec-WebSocket-Version", "8")
          ]
        )
        ""

-- | HTTP response parser
parseResponse :: Parser Response
parseResponse = Response
    <$> fmap (read . BC.unpack) code
    <*> message
    <*> A.manyTill header newline
    <*> A.takeByteString
  where
    space = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    code = A.string "HTTP/1.1" *> space *> A.takeWhile1 (/= c2w ' ') <* space
    message = A.takeWhile1 (/= c2w '\r') <* newline
    header = (,)
        <$> (CI.mk <$> A.takeWhile1 (/= c2w ':'))
        <*  A.string ": "
        <*> A.takeWhile1 (/= c2w '\r')
        <*  newline

-- | Request encoder
encodeRequestBody :: Encoder p RequestBody
encodeRequestBody _ (RequestBody (RequestHttpPart path headers) body) = 
    Builder.copyByteString "GET "      `mappend`
    Builder.copyByteString path        `mappend`
    Builder.copyByteString " HTTP/1.1" `mappend`
    Builder.fromByteString "\r\n"      `mappend`
    mconcat (map header headers)       `mappend`
    Builder.copyByteString "\r\n"      `mappend`
    Builder.copyByteString body
  where
    header (k, v) = mconcat $ map Builder.copyByteString
        [CI.original k, ": ", v, "\r\n"]
