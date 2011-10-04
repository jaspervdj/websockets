-- | Implementation of the WebSocket handshake
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake
    ( HandshakeError (..)
    , handshake
    , response101
    , response400
    , responseError
    , tryFinishRequest
    , receiveClientHandshake
    ) where

import Data.Monoid (mappend, mconcat)
import Control.Monad.Error (Error (..), throwError)

import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Types
import Network.WebSockets.Protocol

import Network.WebSockets.Decode

-- todo: move orphan instance
instance Error HandshakeError where
    -- noMsg  = OtherError ""
    strMsg = OtherError

-- | Provides the logic for the initial handshake defined in the WebSocket
-- protocol. This function will provide you with a 'Response' which accepts and
-- upgrades the received 'Request'. Once this 'Response' is sent, you can start
-- sending and receiving actual application data.
--
-- In the case of a malformed request, a 'HandshakeError' is returned.
handshake :: Request -> Either HandshakeError Response
handshake (Request _ headers _ _) = do
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
            strMsg $ "Header missing: " ++ BC.unpack (CI.original k)  -- todo: will be moved anyway.

-- | Receives and checks the client handshake.
-- 
-- * If this fails, we encountered a syntax error while processing the client's
-- request. That is very bad.
-- TODO: The resulting error will be lifted to the iteratee. What's gonna happen then?
-- 
-- * If it returns @Left@, we either don't support the protocol requested by
-- the client ('NotSupported') or the data the client sent doesn't match the
-- protocol ('MalformedRequest')
--
-- * Otherwise, we are guaranteed that the client handshake is valid and have
-- generated a response ready to be sent back.

-- todo: not used.
receiveClientHandshake :: Decoder (Either HandshakeError Request)
receiveClientHandshake = request >>= tryFinishRequest

-- | Given the HTTP part, try the available protocols one by one.
tryFinishRequest :: RequestHttpPart -> Decoder (Either HandshakeError Request)
-- todo: auto-check if the "Version" header matches? (if any)
tryFinishRequest httpReq = myChoice $ map (($httpReq) . finishRequest) protocols
    where myChoice []     = return . Left $ NotSupported
          myChoice (p:ps) = p >>= \res -> case res of
            e@(Left NotSupported) -> myChoice ps
            x -> return x

-- | An upgrade response
response101 :: Headers -> Response
response101 headers = Response 101 "WebSocket Protocol Handshake" `flip` "" $
    ("Upgrade", "WebSocket") :
    ("Connection", "Upgrade") :
    headers

-- | Bad request
response400 :: Headers -> Response
response400 headers = Response 400 "Bad Request" headers ""

-- | Respond to errors encountered during handshake
responseError :: HandshakeError -> Response
responseError err = response400 $ case err of
    -- List available versions ("version negotiation")
    NotSupported -> [("Sec-WebSocket-Version", B.intercalate ", " $ map version protocols)]
    _ -> []

