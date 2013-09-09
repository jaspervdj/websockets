--------------------------------------------------------------------------------
-- | HTTP utilities
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Tests.Util.Http
    ( ExampleRequest (..)
    , decodeResponseBody
    ) where


--------------------------------------------------------------------------------
import Control.Applicative ((<$>), (<*>))
import qualified Data.Attoparsec as A


--------------------------------------------------------------------------------
import Network.WebSockets.Http


--------------------------------------------------------------------------------
class ExampleRequest p where
    exampleRequest :: p -> RequestBody


--------------------------------------------------------------------------------
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
