-- | This is a simple utility module to implement a publish-subscribe pattern.
-- Note that this only allows communication in a single direction: pusing data
-- from the server to connected clients (browsers).
--
-- Usage:
--
-- * Create a new 'PubSub' handle using 'newPubSub'
--
-- * Subscribe your clients using the 'subscribe' call
--
-- * Push new updates from the server using the 'publish' call
--
{-# LANGUAGE Rank2Types #-}
module Network.WebSockets.Util.PubSub
    ( PubSub
    , newPubSub
    , publish
    , subscribe
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, forever)
import Control.Monad.Trans (liftIO)
import Data.IntMap (IntMap)
import qualified Control.Concurrent.MVar as MV

import qualified Data.IntMap as IM

import Network.WebSockets

data PubSub_ p = PubSub_
    { pubSubNextId :: Int
    , pubSubSinks  :: IntMap (Sink p)
    }

addClient :: Sink p -> PubSub_ p -> (PubSub_ p, Int)
addClient sink (PubSub_ nid sinks) =
    (PubSub_ (nid + 1) (IM.insert nid sink sinks), nid)

removeClient :: Int -> PubSub_ p -> PubSub_ p
removeClient ref ps = ps {pubSubSinks = IM.delete ref (pubSubSinks ps)}

-- | A handle which keeps track of subscribed clients
newtype PubSub p = PubSub (MV.MVar (PubSub_ p))

-- | Create a new 'PubSub' handle, with no clients initally connected
newPubSub :: IO (PubSub p)
newPubSub = PubSub <$> MV.newMVar PubSub_
    { pubSubNextId  = 0
    , pubSubSinks  = IM.empty
    }

-- | Broadcast a message to all connected clients
publish :: PubSub p -> Message p -> IO ()
publish (PubSub mvar) msg = do
    sinks <- pubSubSinks <$> MV.readMVar mvar 
    forM_ (IM.toList sinks) $ \(_, s) -> sendSink s msg

-- | Blocks forever
subscribe :: Protocol p => PubSub p -> WebSockets p ()
subscribe (PubSub mvar) = do
    sink <- getSink
    ref  <- liftIO $ MV.modifyMVar mvar $ return . addClient sink
    catchWsError loop $ const $ liftIO $
        MV.modifyMVar_ mvar $ return . removeClient ref
  where
    loop = forever $ do
        _ <- receiveDataMessage
        return ()
