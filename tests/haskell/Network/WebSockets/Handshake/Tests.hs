{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
    , testCase "handshakeHybi10" handshakeHybi10
    ]

testHandshake :: forall p. Protocol p => p -> B.ByteString -> IO Response
testHandshake _ rq = do
    ia <- newIterAccum
    E.run_ $ E.enumList 1 [rq] $$ runWebSocketsHandshake app (getIter ia)
    Right rsp <- A.parseOnly parseResponse . B.concat <$> getAccum ia
    return rsp
  where
    app :: Request -> WebSockets p ()
    app = sendResponse . requestResponse

(!) :: Eq a => [(a, b)] -> a -> b
assoc ! key = fromJust (lookup key assoc)

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
handshakeHybi00 = testHandshake (undefined :: Hybi00) rq00 >>=
    \(Response code message headers body) -> assert $
        code == 101 &&
        message == "WebSocket Protocol Handshake" &&
        headers ! "Sec-WebSocket-Location" == "ws://example.com/demo" &&
        headers ! "Sec-WebSocket-Origin" == "http://example.com" &&
        body == "8jKS'y:G*Co,Wxa-"

-- The sample from the -10 spec.
rq10 :: B.ByteString
rq10 =
    "GET /chat HTTP/1.1\r\n\
    \Host: server.example.com\r\n\
    \Upgrade: websocket\r\n\
    \Connection: Upgrade\r\n\
    \Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
    \Sec-WebSocket-Origin: http://example.com\r\n\
    \Sec-WebSocket-Protocol: chat, superchat\r\n\
    \Sec-WebSocket-Version: 8\r\n\r\n"

handshakeHybi10 :: Assertion
handshakeHybi10 = testHandshake (undefined :: Hybi10) rq10 >>=
    \(Response code message headers body) -> assert $
        code == 101 &&
        message == "WebSocket Protocol Handshake" &&
        headers ! "Sec-WebSocket-Accept" == "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" &&
        body == ""
