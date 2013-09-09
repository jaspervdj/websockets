--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO)
import           Control.Exception              (handle)
import           Data.ByteString.Char8          ()
import           Data.Maybe                     (fromJust)
import qualified System.IO.Streams.Attoparsec   as Streams
import qualified System.IO.Streams.Builder      as Streams
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Handshake.Test"
    [ testCase "handshake Hybi13"   testHandshakeHybi13
    , testCase "handshake reject"   testHandshakeReject
    , testCase "handshake Hybi9000" testHandshakeHybi9000
    ]


--------------------------------------------------------------------------------
testHandshake :: RequestHead -> (PendingConnection -> IO a) -> IO ResponseHead
testHandshake rq app = do
    (is, os) <- makeChanPipe
    os'      <- Streams.builderStream os
    _        <- forkIO $ app (PendingConnection rq is os') >> return ()
    Streams.parseFromStream decodeResponseHead is


--------------------------------------------------------------------------------
(!) :: Eq a => [(a, b)] -> a -> b
assoc ! key = fromJust (lookup key assoc)


--------------------------------------------------------------------------------
rq13 :: RequestHead
rq13 = RequestHead "/mychat"
    [ ("Host", "server.example.com")
    , ("Upgrade", "websocket")
    , ("Connection", "Upgrade")
    , ("Sec-WebSocket-Key", "x3JJHMbDL1EzLkh9GBhXDw==")
    , ("Sec-WebSocket-Protocol", "chat")
    , ("Sec-WebSocket-Version", "13")
    , ("Origin", "http://example.com")
    ]
    False


--------------------------------------------------------------------------------
testHandshakeHybi13 :: Assertion
testHandshakeHybi13 = do
    ResponseHead code message headers <- testHandshake rq13 acceptRequest

    code @?= 101
    message @?= "WebSocket Protocol Handshake"
    headers ! "Sec-WebSocket-Accept" @?= "HSmrc0sMlYUkAGmm5OPpG2HaGWk="
    headers ! "Connection"           @?= "Upgrade"


--------------------------------------------------------------------------------
testHandshakeReject :: Assertion
testHandshakeReject = do
    ResponseHead code _ _ <- testHandshake rq13 $ \pc ->
        rejectRequest pc "YOU SHALL NOT PASS"

    code @?= 400


--------------------------------------------------------------------------------
-- I don't believe this one is supported yet
rq9000 :: RequestHead
rq9000 = RequestHead "/chat"
    [ ("Host", "server.example.com")
    , ("Upgrade", "websocket")
    , ("Connection", "Upgrade")
    , ("Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ==")
    , ("Sec-WebSocket-Origin", "http://example.com")
    , ("Sec-WebSocket-Protocol", "chat, superchat")
    , ("Sec-WebSocket-Version", "9000")
    ]
    False


--------------------------------------------------------------------------------
testHandshakeHybi9000 :: Assertion
testHandshakeHybi9000 = do
    ResponseHead code _ headers <- testHandshake rq9000 $ \pc ->
        flip handle (acceptRequest pc) $ \e -> case e of
            NotSupported -> return undefined
            _            -> error $ "Unexpected Exception: " ++ show e

    code @?= 400
    headers ! "Sec-WebSocket-Version" @?= "13"
