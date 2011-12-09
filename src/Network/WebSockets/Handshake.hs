-- | Implementation of the WebSocket handshake
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.WebSockets.Handshake
    ( HandshakeError (..)
    , responseError
    , tryFinishRequest
    ) where

import qualified Data.ByteString as B

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Protocol
import Network.WebSockets.Types

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
--
tryFinishRequest :: Protocol p
                 => RequestHttpPart
                 -> Decoder p (Either HandshakeError (Request, p))
tryFinishRequest httpReq = tryInOrder implementations
    -- NOTE that the protocols are tried in order, the first one first. So that
    -- should be the latest one. (only matters if we have overlaps in specs,
    -- though)
  where
    tryInOrder []       = return . Left $ NotSupported
    tryInOrder (p : ps) = finishRequest p httpReq >>= \res -> case res of
        (Left NotSupported) -> tryInOrder ps
        (Left e)            -> return (Left e)
        (Right req)         -> return . Right $ (req, p)

-- | Respond to errors encountered during handshake. First argument may be
-- bottom.
responseError :: forall p. Protocol p => p -> HandshakeError -> Response
responseError _ err = response400 $ case err of
    -- TODO: fix
    NotSupported -> versionHeader  -- Version negotiation
    _            -> []
  where
    versionHeader = [("Sec-WebSocket-Version",
        B.intercalate ", " $ concatMap headerVersions (implementations :: [p]))]
