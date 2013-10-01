--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
module Network.WebSockets.Debug
    ( debug
    ) where


--------------------------------------------------------------------------------
#ifdef DEBUG
import           System.IO (hPutStrLn, stderr)
#endif


--------------------------------------------------------------------------------
debug :: String -> IO ()
#ifdef DEBUG
debug   = hPutStrLn stderr
#else
debug _ = return ()
#endif
{-# INLINE debug #-}
