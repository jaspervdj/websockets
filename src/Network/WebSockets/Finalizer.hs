--------------------------------------------------------------------------------
module Network.WebSockets.Finalizer
    ( Finalizer
    , mkFinalizer
    , emptyFinalizer
    , finalize
    ) where


--------------------------------------------------------------------------------
import           Control.Monad            (when)
import           Data.IORef               (IORef, atomicModifyIORef,
                                           mkWeakIORef, newIORef)
import           System.IO.Unsafe         (unsafePerformIO)


--------------------------------------------------------------------------------
import           Network.WebSockets.Debug


--------------------------------------------------------------------------------
data Finalizer = Finalizer (IORef Bool) (IO ())


--------------------------------------------------------------------------------
mkFinalizer :: IO () -> IO Finalizer
mkFinalizer action = do
    done <- newIORef False
    let finalizer = Finalizer done action
    _ <- mkWeakIORef done (finalize finalizer)
    return finalizer


--------------------------------------------------------------------------------
-- | A dummy finalizer that doesn't do anything and has no overhead.
emptyFinalizer :: Finalizer
emptyFinalizer = Finalizer
    (unsafePerformIO $ newIORef True)
    (error "Network.WebSockets.Finalizer.emptyFinalizer")


--------------------------------------------------------------------------------
finalize :: Finalizer -> IO ()
finalize (Finalizer done action) = do
    needToClose <- atomicModifyIORef done $ \done' ->
        if done' then (True, False) else (True, True)
    debug $ if needToClose
        then "Network.WebSockets.Finalizer.finalizer: Finalizing"
        else "Network.WebSockets.Finalizer.finalizer: Already finalized"
    when needToClose action
