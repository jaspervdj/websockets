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
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator as E

import Network.WebSockets
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Tests.Util.Http
import Network.WebSockets.Tests.Util.IterAccum

tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testCase "handshakeHybi00"   handshakeHybi00
    , testCase "handshakeHybi10"   handshakeHybi10
    , testCase "handshakeHybi9000" handshakeHybi9000
    , testCase "handshakeReject"   handshakeReject
    ]

testHandshake :: forall p. Protocol p
              => (Request -> WebSockets p ()) -> p -> RequestBody -> IO Response
testHandshake app _ rq = do
    ia <- newIterAccum
    -- Encode request
    let bs = B.concat $ BL.toChunks $ Builder.toLazyByteString $
                encodeRequestBody Nothing rq
    -- Ignore possible error, we can inspect it using the response anyway
    _ <- E.run $ E.enumList 1 [bs] $$ runWebSocketsHandshake app (getIter ia)
    Right rsp <- A.parseOnly parseResponse . B.concat <$> getAccum ia
    return rsp

testHandshakeAccept :: Protocol p => p -> RequestBody -> IO Response
testHandshakeAccept = testHandshake acceptRequest

(!) :: Eq a => [(a, b)] -> a -> b
assoc ! key = fromJust (lookup key assoc)

-- The sample from the -00 spec.
rq00 :: RequestBody
rq00 = exampleRequest Hybi00_

handshakeHybi00 :: Assertion
handshakeHybi00 = testHandshakeAccept (undefined :: Hybi00) rq00 >>=
    \(Response code message headers body) -> assert $
        code == 101 &&
        message == "WebSocket Protocol Handshake" &&
        headers ! "Sec-WebSocket-Location" == "ws://example.com/demo" &&
        headers ! "Sec-WebSocket-Origin" == "http://example.com" &&
        body == "8jKS'y:G*Co,Wxa-"

-- The sample from the -10 spec.
rq10 :: RequestBody
rq10 = exampleRequest Hybi10_

handshakeHybi10 :: Assertion
handshakeHybi10 = testHandshakeAccept (undefined :: Hybi10) rq10 >>=
    \(Response code message headers body) -> assert $
        code == 101 &&
        message == "WebSocket Protocol Handshake" &&
        headers ! "Sec-WebSocket-Accept" == "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" &&
        body == ""

-- I don't believe this one is supported yet
rq9000 :: RequestBody
rq9000 = RequestBody
    ( RequestHttpPart "/chat"
      [ ("Host", "server.example.com")
      , ("Upgrade", "websocket")
      , ("Connection", "Upgrade")
      , ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
      , ("Sec-WebSocket-Origin", "http://example.com")
      , ("Sec-WebSocket-Protocol", "chat, superchat")
      , ("Sec-WebSocket-Version", "9000")
      ]
    )
    ""

handshakeHybi9000 :: Assertion
handshakeHybi9000 = testHandshakeAccept (undefined :: Hybi00) rq9000 >>=
    \(Response code _ headers body) -> assert $
        code == 400 &&
        headers ! "Sec-WebSocket-Version" == "0, 8" &&
        body == ""

handshakeReject :: Assertion
handshakeReject = testHandshake (flip rejectRequest "YOU SHALL NOT PASS")
    (undefined :: Hybi00) rq9000 >>=
        \(Response code _ _ body) -> assert $
            code == 400 && body == ""
