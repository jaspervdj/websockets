-- | Provides the 'WebSocket' type
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.WebSocket
    ( WebSocket
    , new
    , close
    , receive
    , send
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Network.Socket (Socket, sClose)
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Blaze.ByteString.Builder (toLazyByteString)

import Network.WebSockets.Decode (Result (..), Decoder)
import Network.WebSockets.Encode (Encoder)

-- | Holds the state of a websocket, with an internal buffer
data WebSocket = WebSocket
    { buffer :: IORef ByteString
    , socket :: Socket
    }

-- | Create a new websocket
new :: Socket -> IO WebSocket
new s = do
    b <- newIORef ""
    return $ WebSocket {buffer = b, socket = s}

-- | Close a websocket
close :: WebSocket -> IO ()
close = sClose . socket

-- | Parse a message from a websocket
receive :: Decoder a -> WebSocket -> IO (Maybe a)
receive decoder (WebSocket br s) = readIORef br >>= receive'
  where
    bufferSize = 4096
    receive' "" = recv s bufferSize >>= receive'
    receive' b = case decoder b of
        Ok x r     -> writeIORef br r >> return (Just x)
        Error      -> sClose s >> return Nothing
        Incomplete -> do
            putStrLn "Receiving..."
            b' <- fmap (b `B.append`) $ recv s 4096
            receive' b'

-- | Unparse and send
send :: Encoder a -> WebSocket -> a -> IO ()
send encoder ws x = sendAll (socket ws) (toLazyByteString $ encoder x)
