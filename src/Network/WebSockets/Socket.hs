{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Socket
    ( runWithSocket
    ) where

import Control.Monad.Trans (liftIO)
import Network.Socket (Socket, sClose)

import Network.Socket.ByteString (recv, sendMany)

import Data.ByteString (ByteString)
import Data.Enumerator ( Enumerator, Iteratee (..), Stream (..)
                       , checkContinue0, continue, yield, (>>==)
                       )

import Network.WebSockets.Monad

runWithSocket :: Socket -> WebSockets a -> IO a
runWithSocket socket ws = do
    r <- runWebSockets ws (receiveEnum socket) (sendIter socket)
    either (error . show) return r

receiveEnum :: Socket -> Enumerator ByteString IO a
receiveEnum socket = checkContinue0 $ \loop f -> do
    b <- liftIO $ recv socket 4096
    if b == ""
        then liftIO (sClose socket) >> continue f
        else f (Chunks [b]) >>== loop

sendIter :: Socket -> Iteratee ByteString IO ()
sendIter socket = continue go
  where
    go (Chunks []) = continue go
    go (Chunks cs) = liftIO (sendMany socket cs) >> continue go
    go EOF         = liftIO (sClose socket) >> yield () EOF
