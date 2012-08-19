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

import Blaze.ByteString.Builder (Builder)
import Data.Attoparsec (Parser)
import Data.ByteString.Internal (c2w)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal

class ExampleRequest p where
    exampleRequest :: p -> RequestBody

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
          False
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
          False
        )
        ""
