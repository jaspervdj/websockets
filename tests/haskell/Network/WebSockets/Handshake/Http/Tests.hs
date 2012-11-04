--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake.Http.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8          as BC
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert)


--------------------------------------------------------------------------------
import Network.WebSockets.Handshake.Http


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Handshake.Http.Tests"
    [ testCase "jwebsockets response" jWebSocketsResponse
    ]


--------------------------------------------------------------------------------
jWebSocketsResponse :: Assertion
jWebSocketsResponse = assert $ case A.parseOnly decodeResponse input of
    Left err -> error err
    Right _  -> True
  where
    input = BC.intercalate "\r\n"
        [ "HTTP/1.1 101 Switching Protocols"
        , "Upgrade: websocket"
        , "Connection: Upgrade"
        , "Sec-WebSocket-Accept: Ha0QR1T9CoYx/nqwHsVnW8KVTSo="
        , "Sec-WebSocket-Origin: "
        , "Sec-WebSocket-Location: ws://127.0.0.1"
        , "Set-Cookie: JWSSESSIONID=2e0690e2e328f327056a5676b6a890e3; HttpOnly"
        , ""
        , ""
        ]
