-- | How do you use this library? Here's how:
--
-- * Get an enumerator/iteratee pair from your favorite web server, or use
--   'I.runServer' to set up a simple standalone server.
--
-- * Read the 'I.Request' using 'receiveRequest'. Inspect its path and the perform
--   the initial 'H.handshake'. This yields a 'I.Response' which you can send
--   back using 'sendResponse'. The WebSocket is now ready.
--
-- * Use functions like 'receiveTextData' and 'sendTextData' to do simple,
--   sequential communication with the client.
--
-- * 'I.getSender' allows you obtain a function with which you can send data to
--   the client asynchronously.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Control.Monad.Trans (liftIO)
-- > import qualified Data.Text as T
-- >
-- > import Network.WebSockets
-- >
-- > -- Accepts clients, spawns a single handler for each one.
-- > main :: IO ()
-- > main = runServer "0.0.0.0" 8000 $ do
-- >     Just request <- receiveRequest
-- >     case handshake request of
-- >         Left err  -> liftIO $ print err
-- >         Right rsp -> do
-- >             sendResponse rsp
-- >             sendTextData "Do you read me, Lieutenant Bowie?"
-- >             liftIO $ putStrLn "Shook hands, sent welcome message."
-- >             talkLoop
-- >
-- > -- Talks to the client (by echoing messages back) until EOF.
-- > talkLoop :: WebSockets ()
-- > talkLoop = do
-- >     msg <- receiveTextData
-- >     case msg of
-- >         Nothing -> liftIO $ putStrLn "EOF encountered, quitting"
-- >         Just m  -> do
-- >             sendTextData $ m `T.append` ", meow."
-- >             talkLoop
module Network.WebSockets
    ( 
      -- * WebSocket type
      I.WebSockets
    , I.runWebSockets

      -- * A simple standalone server
    , I.runServer
    , I.runWithSocket

      -- * Types
    , I.Headers
    , I.Request (..)
    , I.Response (..)
    , I.Frame (..)

      -- * Initial handshake
    , H.HandshakeError (..)
    , receiveRequest
    , sendResponse
    , H.handshake

      -- * Sending and receiving
    , receiveFrame
    , receiveByteStringData
    , receiveTextData
    , I.send
    , sendByteStringData
    , sendTextData

      -- * Advanced sending
    , E.Encoder
    , I.Sender
    , I.getSender
    , E.response
    , E.byteStringData
    , E.textData
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Encode as E
import qualified Network.WebSockets.Handshake as H
import qualified Network.WebSockets.Monad as I
import qualified Network.WebSockets.Socket as I
import qualified Network.WebSockets.Types as I

-- | Read a 'I.Request' from the socket. Blocks until one is received and
-- returns 'Nothing' if the socket has been closed.
receiveRequest :: I.WebSockets (Maybe I.Request)
receiveRequest = I.receive D.request

-- | Send a 'I.Response' to the socket immediately.
sendResponse :: I.Response -> I.WebSockets ()
sendResponse = I.send E.response

-- | Read a 'I.Frame' from the socket. Blocks until a frame is received and
-- returns 'Nothing' if the socket has been closed.
--
-- Note that a typical library user will want to use something like
-- 'receiveByteStringData' instead.
receiveFrame :: I.WebSockets (Maybe I.Frame)
receiveFrame = I.receive D.frame

-- | Read frames from the socket, automatically responding:
--
-- * On a close operation, close the socket and return 'Nothing'
--
-- * On a ping, send a pong
--
--
-- This function thus block until a data frame is received. A return value of
-- 'Nothing' means the socket has been closed.
receiveByteStringData :: I.WebSockets (Maybe ByteString)
receiveByteStringData = do
    frame <- receiveFrame
    case frame of
        Nothing         -> return Nothing
        Just (I.Data x) -> return (Just x)
        Just I.Close    -> return Nothing
        -- TODO: send pong & recurse
        Just I.Ping     -> error "TODO"
        Just I.Pong     -> error "TODO"

-- | A higher-level variant of 'receiveByteStringData' which does the decoding
-- for you.
receiveTextData :: I.WebSockets (Maybe Text)
receiveTextData = (fmap . fmap) TE.decodeUtf8 receiveByteStringData

-- | Send a 'ByteString' to the socket immediately.
sendByteStringData :: ByteString -> I.WebSockets ()
sendByteStringData = I.send E.byteStringData

-- | A higher-level variant of 'sendByteStringData' which does the encoding for
-- you.
sendTextData :: Text -> I.WebSockets ()
sendTextData = I.send E.textData
