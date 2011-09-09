-- | Provides the 'WebSocket' type
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.WebSocket
    ( WebSocket
    , new
    , receive
    , sendRaw
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import Network.Socket (Socket, sClose)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Network.WebSockets.Parse (Result (..), Parser)

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

-- | Parse a message from a websocket
receive :: Parser a -> WebSocket -> IO (Maybe a)
receive parser (WebSocket br s) = readIORef br >>= receive'
  where
    bufferSize = 4096
    receive' "" = recv s bufferSize >>= receive'
    receive' b = case parser b of
        Ok x r     -> writeIORef br r >> return (Just x)
        Error      -> sClose s >> return Nothing
        Incomplete -> do
            putStrLn "Receiving..."
            b' <- fmap (b `B.append`) $ recv s 4096
            receive' b'

-- | Send some raw data
sendRaw :: WebSocket -> ByteString -> IO ()
sendRaw ws = sendAll (socket ws)
