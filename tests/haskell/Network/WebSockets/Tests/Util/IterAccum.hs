-- | Utility for simulating iteratee output and capturing it
module Network.WebSockets.Tests.Util.IterAccum
    ( IterAccum
    , newIterAccum
    , getIter
    , getAccum
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Data.Enumerator (Iteratee)
import qualified Data.Enumerator as E

newtype IterAccum a = IterAccum {unIterAccum :: IORef [a]}

newIterAccum :: IO (IterAccum a)
newIterAccum = IterAccum <$> newIORef []

getIter :: IterAccum a -> Iteratee a IO ()
getIter (IterAccum ref) = E.continue go
  where
    go (E.Chunks cs) = liftIO (forM_ cs add) >> E.continue go
    go E.EOF         = E.yield () E.EOF

    add x = do
        xr <- readIORef ref
        writeIORef ref $ x : xr

getAccum :: IterAccum a -> IO [a]
getAccum = fmap reverse . readIORef . unIterAccum
