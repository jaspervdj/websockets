--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO)
import           Control.Exception              (handle)
import           Data.ByteString.Char8          ()
import           Data.IORef                     (newIORef, readIORef,
                                                 writeIORef)
import           Data.Maybe                     (fromJust)
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert, (@?=))


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import qualified Network.WebSockets.Stream      as Stream


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Handshake.Test"
    [ testCase "handshake Hybi13"                   testHandshakeHybi13
    , testCase "handshake Hybi13 with subprotocols" testHandshakeHybi13WithProto
    , testCase "handshake reject"                   testHandshakeReject
    , testCase "handshake Hybi9000"                 testHandshakeHybi9000
    ]


--------------------------------------------------------------------------------
testHandshake :: RequestHead -> (PendingConnection -> IO a) -> IO ResponseHead
testHandshake rq app = do
    echo <- Stream.makeEchoStream
    _    <- forkIO $ do
        _ <- app (PendingConnection defaultConnectionOptions rq nullify echo)
        return ()
    mbRh <- Stream.parse echo decodeResponseHead
    Stream.close echo
    case mbRh of
        Nothing -> fail "testHandshake: No response"
        Just rh -> return rh
  where
    nullify _ = return ()


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
    , ("Sec-WebSocket-Protocol", "chat, superchat")
    , ("Sec-WebSocket-Version", "13")
    , ("Origin", "http://example.com")
    ]
    False


--------------------------------------------------------------------------------
testHandshakeHybi13 :: Assertion
testHandshakeHybi13 = do
    onAcceptFired                     <- newIORef False
    ResponseHead code message headers <- testHandshake rq13 $ \pc ->
        acceptRequest pc {pendingOnAccept = \_ -> writeIORef onAcceptFired True}

    readIORef onAcceptFired >>= assert
    code @?= 101
    message @?= "WebSocket Protocol Handshake"
    headers ! "Sec-WebSocket-Accept" @?= "HSmrc0sMlYUkAGmm5OPpG2HaGWk="
    headers ! "Connection"           @?= "Upgrade"
    lookup "Sec-WebSocket-Protocol" headers @?= Nothing

--------------------------------------------------------------------------------
testHandshakeHybi13WithProto :: Assertion
testHandshakeHybi13WithProto = do
    onAcceptFired                     <- newIORef False
    ResponseHead code message headers <- testHandshake rq13 $ \pc -> do
        getRequestSubprotocols (pendingRequest pc) @?= ["chat", "superchat"]
        acceptRequestWith pc {pendingOnAccept = \_ -> writeIORef onAcceptFired True}
                          (AcceptRequest $ Just "superchat")

    readIORef onAcceptFired >>= assert
    code @?= 101
    message @?= "WebSocket Protocol Handshake"
    headers ! "Sec-WebSocket-Accept" @?= "HSmrc0sMlYUkAGmm5OPpG2HaGWk="
    headers ! "Connection"           @?= "Upgrade"
    headers ! "Sec-WebSocket-Protocol" @?= "superchat"

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
