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

       -- * Route by subprotocol
    , subprotocol
    , noSubprotocols

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
import           System.FilePath -- (makeRelative, splitDirectories)

-- | The main routing monad.
newtype WebSocketsRoute a = WebSocketsRoute
    { unWebSocketsRoute :: ReaderT (PendingConnection, Maybe ByteString) IO a
    } deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

-- | Route websocket requests. If no route is accepted the connection will be
-- rejected.
routeWebSockets :: WebSocketsRoute () -> ServerApp
routeWebSockets route pending =
    runReaderT (unWebSocketsRoute (route <|> reject)) (pending, Nothing)
  where
    reject = liftIO $ rejectRequest pending ""

-- | Get the current pending connection. Note that certain functions like e.g.
-- 'dir' may modify the 'RequestHead' on subroutes.
askPending :: WebSocketsRoute PendingConnection
askPending = WebSocketsRoute $ asks fst

-- helper
askSubprotocol :: WebSocketsRoute (Maybe ByteString)
askSubprotocol = WebSocketsRoute $ asks snd

--------------------------------------------------------------------------------
-- Helper functions

withPending :: (PendingConnection -> PendingConnection) -> WebSocketsRoute a -> WebSocketsRoute a
withPending f r = WebSocketsRoute $ withReaderT (\(p,s) -> (f p,s)) (unWebSocketsRoute r)

setProto :: Maybe ByteString -> WebSocketsRoute a -> WebSocketsRoute a
setProto bs r = WebSocketsRoute $ withReaderT (\(p,_) -> (p,bs)) (unWebSocketsRoute r)

setPath :: [String] -> PendingConnection -> PendingConnection
setPath bs pending =
    pending { pendingRequest = (pendingRequest pending) { requestPath = unpaths bs } }

paths :: PendingConnection -> [String]
paths = splitPath . makeRelative "/" . B8.unpack . requestPath . pendingRequest

unpaths :: [String] -> ByteString
unpaths = B8.pack . joinPath

--------------------------------------------------------------------------------
-- Accept requests

-- | Accept a request. If a route has been chosen using 'subprotocol' the
-- corresponding subprotocol will be selected for the connection.
--
-- Every request should only be accepted once.
routeAccept :: (Connection -> IO a) -> WebSocketsRoute a
routeAccept go = do
    proto <- askSubprotocol
    routeAcceptWith (AcceptRequest proto) go

-- | Accept a request and overwrite the default sub protocol and using the
-- given 'AcceptRequest' instead.
routeAcceptWith :: AcceptRequest -> (Connection -> IO a) -> WebSocketsRoute a
routeAcceptWith req go = do
    pending <- askPending
    liftIO $ go =<< acceptRequestWith pending req

--------------------------------------------------------------------------------
-- Route by subprotocol

-- | Route by available subprotocols. If any of the protocols in the header
-- matches the route will succeed.
--
-- > route :: WebSocketsRoute ()
-- > route = msum [ subprotocol "json" $ jsonRoute
-- >              , subprotocol "html" $ htmlRoute ]
--
-- Calling 'routeAccept' on the inner route will automatically send the
-- selected subprotocol to the client.
subprotocol :: ByteString -> WebSocketsRoute a -> WebSocketsRoute a
subprotocol proto w = do
    p <- askPending
    let reqProto = getRequestSubprotocols . pendingRequest $ p
    if proto `elem` reqProto then
        setProto (Just proto) w
      else
        mzero

-- | Guard which only succeeds if no subprotocols have been requested.
noSubprotocols :: WebSocketsRoute ()
noSubprotocols = do
    p <- askPending
    guard $ null . getRequestSubprotocols . pendingRequest $ p

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
        (p':ps) | p == dropTrailingPathSeparator p' -> do
            let ps' | hasTrailingPathSeparator p' = "/" : ps
                    | otherwise                   =       ps
            withPending (setPath ps') r
        _ -> mzero

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
    case B8.unsnoc . requestPath . pendingRequest $ pending of
        Just (_, '/') -> return ()
        _             -> mzero

-- | The opposite of 'trailingSlash'
noTrailingSlash :: WebSocketsRoute ()
noTrailingSlash = do
    pending <- askPending
    case B8.unsnoc . requestPath . pendingRequest $ pending of
        Just (_, '/') -> mzero
        _             -> return ()

-- | Pop any path element and run the websocket route. Succeeds if a path
-- component was popped. Fails if the remaining path was empty.
path :: (String -> WebSocketsRoute a) -> WebSocketsRoute a
path r = do
    pending <- askPending
    case paths pending of
        (p:ps) | p /= "." -> -- makeRelative turns "/" into ["."]
            withPending (setPath ps) (r $ dropTrailingPathSeparator p)
        _ -> mzero
