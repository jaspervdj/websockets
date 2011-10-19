{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake.Tests
    ( tests
    ) where

import Control.Applicative ((<$>))
import Data.ByteString.Char8 ()
import Data.Enumerator (($$))
import Data.Maybe (fromJust)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assert)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.Enumerator as E

import Network.WebSockets
import Network.WebSockets.Tests.Util.Http
import Network.WebSockets.Tests.Util.IterAccum

tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testCase "handshakeHybi00" handshakeHybi00
    ]

-- The sample from the -00 spec.
rq00 :: B.ByteString
rq00 =
    "GET /demo HTTP/1.1\r\n\
    \Host: example.com\r\n\
    \Connection: Upgrade\r\n\
    \Sec-WebSocket-Key2: 12998 5 Y3 1  .P00\r\n\
    \Sec-WebSocket-Protocol: sample\r\n\
    \Upgrade: WebSocket\r\n\
    \Sec-WebSocket-Key1: 4 @1  46546xW%0l 1 5\r\n\
    \Origin: http://example.com\r\n\r\n\
    \^n:ds[4U"

handshakeHybi00 :: Assertion
handshakeHybi00 = do
    ia <- newIterAccum
    E.run_ $ E.enumList 1 [rq00] $$ runWebSocketsHandshake app (getIter ia)
    Right rsp <- A.parseOnly parseResponse . B.concat <$> getAccum ia
    let Response code message headers body = rsp
    assert $
        code == 101 &&
        message == "WebSocket Protocol Handshake" &&
        headers ! "Sec-WebSocket-Location" == "ws://example.com/demo" &&
        headers ! "Sec-WebSocket-Origin" == "http://example.com" &&
        body == "8jKS'y:G*Co,Wxa-"
  where
    app :: Request -> WebSockets Hybi00 ()
    app = sendResponse . requestResponse
    assoc ! key = fromJust (lookup key assoc)
