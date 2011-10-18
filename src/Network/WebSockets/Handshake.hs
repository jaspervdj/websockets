-- | Implementation of the WebSocket handshake
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake
    ( HandshakeError (..)
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
import Network.WebSockets.Http
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi00 (Hybi00 (..))
import Network.WebSockets.Protocol.Hybi10 (Hybi10 (..))


-- | Receives and checks the client handshake.
-- 
-- * If this fails, we encountered a syntax error while processing the client's
-- request. That is very bad.
-- 
-- * If it returns @Left@, we either don't support the protocol requested by
-- the client ('NotSupported') or the data the client sent doesn't match the
-- protocol ('MalformedRequest')
--
-- * Otherwise, we are guaranteed that the client handshake is valid and have
-- generated a response ready to be sent back.

-- | (try to) receive and validate a complete request. Not used at the moment.
receiveClientHandshake :: Protocol p
                       => Decoder p (Either HandshakeError (Request, p))
receiveClientHandshake = decodeRequest >>= tryFinishRequest

-- | Given the HTTP part, try the available protocols one by one.
-- todo: auto-check if the "Version" header matches? (if any)
tryFinishRequest :: Protocol p
                 => RequestHttpPart
                 -> Decoder p (Either HandshakeError (Request, p))
tryFinishRequest httpReq = tryInOrder implementations
    -- NOTE that the protocols are tried in order, the first one first. So that
    -- should be the latest one. (only matters if we have overlaps in specs,
    -- though)
    where
        tryInOrder []     = return . Left $ NotSupported
        tryInOrder (p:ps) = finishRequest p httpReq >>= \res -> case res of
          e@(Left NotSupported) -> tryInOrder ps
          (Left e)              -> return (Left e)  -- not "e@(Left _) -> return e" !
          (Right req)           -> return . Right $ (req, p)

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
responseError :: Protocol p => [p] -> HandshakeError -> Response
responseError protocols err = response400 $ case err of
    -- TODO: fix
    NotSupported -> versionHeader  -- List all available versions ("version negotiation")
    _            -> []
  where
    versionHeader =
      [("Sec-WebSocket-Version", B.intercalate ", " $ map headerVersion $ protocols)]
