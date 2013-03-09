--------------------------------------------------------------------------------
-- | Implementation of the WebSocket handshake
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Network.WebSockets.Handshake
    ( HandshakeError (..)
    , handshake
    , responseError
    ) where


--------------------------------------------------------------------------------
import Data.List (find)
import qualified Data.ByteString as B
import qualified Data.Enumerator as E


--------------------------------------------------------------------------------
import Network.WebSockets.Handshake.Http
import Network.WebSockets.Protocol


--------------------------------------------------------------------------------
-- | Receives and checks the client handshake. If no suitable protocol is found
-- (or the client sends garbage), a 'HandshakeError' will be thrown.
handshake :: (Monad m, Protocol p)
          => RequestHttpPart
          -> E.Iteratee B.ByteString m (Request, p)
handshake rhp = case find (flip supported rhp) implementations of
    Nothing -> E.throwError NotSupported
    Just p  -> do
        rq <- finishRequest p rhp
        return (rq, p)


--------------------------------------------------------------------------------
-- | Respond to errors encountered during handshake. First argument may be
-- bottom.
responseError :: forall p. Protocol p => p -> HandshakeError -> ResponseBody
responseError _ err = response400 $ case err of
    -- TODO: fix
    NotSupported -> versionHeader  -- Version negotiation
    _            -> []
  where
    versionHeader = [("Sec-WebSocket-Version",
        B.intercalate ", " $ concatMap headerVersions (implementations :: [p]))]
