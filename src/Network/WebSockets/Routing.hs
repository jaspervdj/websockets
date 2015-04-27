{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | WebSockets routing for incoming 'PendingConnection'.
module Network.WebSockets.Routing
  ( WebSocketsRoute
  , routeWebSockets
  , askPending

    -- * Accepting a request
  , routeAccept
  , routeAcceptWith

    -- * Route by path info
  , dir
  , dirs
  , nullDir
  , trailingSlash
  , noTrailingSlash
  , path
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Network.WebSockets
import           System.FilePath (makeRelative, splitDirectories)

-- | The main routing monad.
newtype WebSocketsRoute a = WebSocketsRoute
  { unWebSocketsRoute :: ReaderT PendingConnection IO a
  } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

-- | Route websocket requests. If no route is accepted the connection will be
-- rejected.
routeWebSockets :: WebSocketsRoute () -> ServerApp
routeWebSockets route pending =
  runReaderT (unWebSocketsRoute (route <|> reject)) pending
 where
  reject = liftIO $ rejectRequest pending ""

-- | Get the current pending connection
askPending :: WebSocketsRoute PendingConnection
askPending = WebSocketsRoute ask

--------------------------------------------------------------------------------
-- Helper functions

withPending :: (PendingConnection -> PendingConnection) -> WebSocketsRoute a -> WebSocketsRoute a
withPending f r = WebSocketsRoute $ withReaderT f (unWebSocketsRoute r)

setPath :: [String] -> PendingConnection -> PendingConnection
setPath bs pending =
  pending { pendingRequest = (pendingRequest pending) { requestPath = unpaths bs } }

paths :: PendingConnection -> [String]
paths = splitDirectories . makeRelative "/" . B8.unpack . requestPath . pendingRequest

unpaths :: [String] -> ByteString
unpaths = B8.intercalate "/" . map B8.pack

--------------------------------------------------------------------------------
-- Accept requests

-- | Accept a request. Every request should only be accepted once.
routeAccept :: (Connection -> IO a) -> WebSocketsRoute a
routeAccept go = do
  pending <- askPending
  liftIO $ go =<< acceptRequest pending

routeAcceptWith :: AcceptRequest -> (Connection -> IO a) -> WebSocketsRoute a
routeAcceptWith req go = do
  pending <- askPending
  liftIO $ go =<< acceptRequestWith pending req

--------------------------------------------------------------------------------
-- Route by path info

-- | Pop a path element and run the supplied handler if it matches the given
-- string.
--
-- > route :: WebSocketsRoute ()
-- > route = dir "foo" $ dir "bar" $ subRoute
--
-- The path element can not contain \'/\'. See also 'dirs'.
dir :: String -> WebSocketsRoute a -> WebSocketsRoute a
dir p r = do
  pending <- askPending
  case paths pending of
    (p':ps) | p == p' -> withPending (setPath ps) r
    _                 -> mzero

-- | Guard against a 'FilePath'. Unlike 'dir' the 'FilePath' may contain \'/\'.
-- If the guard succeeds, the matched elements will be popped from the directory stack.
--
-- > dirs "foo/bar" $ ...
--
-- See also: 'dir'.
dirs :: FilePath -> WebSocketsRoute a -> WebSocketsRoute a
dirs fp w = do
  let parts = splitDirectories $ makeRelative "/" fp
  foldr dir w parts

-- | Guard which only succeeds if there are no remaining path segments. Often
-- used if you want to explicitly assign a route for \'/\'.
nullDir :: WebSocketsRoute ()
nullDir = do
  pending <- askPending
  guard $ null (paths pending)

-- | Guard which checks that the request URI ends in \'/\'. Useful for
-- distinguishing between @foo@ and @foo/@.
trailingSlash :: WebSocketsRoute ()
trailingSlash = do
  pending <- askPending
  guard $ ('/' ==) . B8.last . requestPath . pendingRequest $ pending

-- | The opposite of 'trailingSlash'
noTrailingSlash :: WebSocketsRoute ()
noTrailingSlash = do
  pending <- askPending
  guard $ ('/' /=) . B8.last . requestPath . pendingRequest $ pending

-- | Pop any path element and run the websocket route. Succeeds if a path
-- component was popped. Fails if the remaining path was empty.
path :: (String -> WebSocketsRoute a) -> WebSocketsRoute a
path r = do
  pending <- askPending
  case paths pending of
    (p:ps) -> withPending (setPath ps) (r p)
    _      -> mzero
