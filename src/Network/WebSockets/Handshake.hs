-- | Implementation of the WebSocket handshake
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake
    ( HandshakeError (..)
    , handshake
    , response101
    , response400
    ) where

import Data.Monoid (mappend, mconcat)
import Control.Monad.Error (Error (..), throwError)

import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Types

instance Error HandshakeError where
    noMsg  = HandshakeError "Handshake error"
    strMsg = HandshakeError

-- | Provides the logic for the initial handshake defined in the WebSocket
-- protocol. This function will provide you with a 'Response' which accepts and
-- upgrades the received 'Request'. Once this 'Response' is sent, you can start
-- sending and receiving actual application data.
--
-- In the case of a malformed request, a 'HandshakeError' is returned.
handshake :: Request -> Either HandshakeError Response
handshake (Request _ headers _) = do
    key <- getHeader "Sec-WebSocket-Key"
    let hash = unlazy $ bytestringDigest $ sha1 $ lazy $ key `mappend` guid
    let encoded = B64.encode hash

    return $ response101 [("Sec-WebSocket-Accept", encoded)]
  where
    guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    lazy = BL.fromChunks . return
    unlazy = mconcat . BL.toChunks
    getHeader k = case lookup k headers of
        Just t  -> return t
        Nothing -> throwError $
            HandshakeError $ "Header missing: " ++ BC.unpack (CI.original k)

-- | An upgrade response
response101 :: Headers -> Response
response101 headers = Response 101 "WebSocket Protocol Handshake" $
    ("Upgrade", "WebSocket") :
    ("Connection", "Upgrade") :
    headers

-- | Bad request
response400 :: Headers -> Response
response400 headers = Response 400 "Bad Request" headers
