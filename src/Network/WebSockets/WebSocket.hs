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
import Blaze.ByteString.Builder (toLazyByteString)
import Data.Attoparsec (Parser, Result (..), parse)

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
receive :: Parser a -> WebSocket -> IO (Maybe a)
receive parser (WebSocket br s) = readIORef br >>= receive' . parse parser
  where
    receive' (Done b x)   = writeIORef br b >> return (Just x)
    receive' (Fail _ _ e) = error e >> sClose s >> return Nothing
    receive' (Partial f)  = recv s 4096 >>= receive' . f

-- | Unparse and send
send :: Encoder a -> WebSocket -> a -> IO ()
send encoder ws x = sendAll (socket ws) (toLazyByteString $ encoder x)
