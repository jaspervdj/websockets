--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Http.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Attoparsec.ByteString     as A
import qualified Data.ByteString.Char8          as BC
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert)


--------------------------------------------------------------------------------
import           Network.WebSockets.Http


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Http.Tests"
    [ testCase "jwebsockets response" jWebSocketsResponse
    , testCase "chromium response"    chromiumResponse
    , testCase "matchbook response"   matchbookResponse
    ]


--------------------------------------------------------------------------------
-- | This is a specific response sent by jwebsockets which caused trouble
jWebSocketsResponse :: Assertion
jWebSocketsResponse = assert $ case A.parseOnly decodeResponseHead input of
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


--------------------------------------------------------------------------------
-- | This is a specific response sent by chromium which caused trouble
chromiumResponse :: Assertion
chromiumResponse = assert $ case A.parseOnly decodeResponseHead input of
    Left err -> error err
    Right _  -> True
  where
    input = BC.intercalate "\r\n"
        [ "HTTP/1.1 500 Internal Error"
        , "Content-Type:text/html"
        , "Content-Length:23"
        , ""
        , "No such target id: 20_1"
        ]

--------------------------------------------------------------------------------
-- | This is a specific response sent by Matchbook.com which caused trouble

matchbookResponse :: Assertion
matchbookResponse = assert $ case A.parseOnly decodeResponseHead input of
    Left err -> error err
    Right _  -> True
  where
    input = BC.intercalate "\r\n"
        [ "HTTP/1.1 101 "
        , "Date: Mon, 22 May 2017 19:39:08 GMT"
        , "Connection: upgrade"
        , "Set-Cookie: __cfduid=deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdea; expires=Tue, 22-May-18 19:39:08 GMT; path=/; domain=.matchbook.com; HttpOnly"
        , "X-Content-Type-Options: nosniff"
        , "X-XSS-Protection: 1; mode=block"
        , "X-Frame-Options: DENY"
        , "Upgrade: websocket"
        , "Sec-WebSocket-Accept: dEadB33fDeadbEEfD3aDbE3Fdea="
        , "X-MB-HA: edge-socket"
        , "X-MB-HAP: haproxy01aws"
        , "Server: cloudflare-nginx"
        , "CF-RAY: 3632deadbeef5b33-HEL"
        , ""
        , ""
        ]
