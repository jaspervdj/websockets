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
    , I.FrameType (..)
    , I.Frame (..)
    , I.Message (..)
    , I.ControlMessage (..)
    , I.DataMessage (..)

      -- * Initial handshake
    , H.HandshakeError (..)
    , receiveRequest
    , sendResponse
    , H.handshake

      -- * Sending and receiving
    , receiveFrame
    , sendFrame
    , receiveMessage
    , receiveDataMessage
    , receiveTextData
    , receiveBinaryData
    -- , receiveByteStringData
    -- , receiveTextData
    , I.send
    -- , sendByteStringData
    -- , sendTextData

      -- * Advanced sending
    , E.Encoder
    , I.Sender
    , I.getSender
    , E.response
    , E.frame
    , E.message
    , E.controlMessage
    , E.dataMessage
    , E.textData
    , E.binaryData
    ) where

import Control.Applicative ((<$>))
import Control.Monad.State (put, get)
import Data.Monoid (mappend, mempty)

import Blaze.ByteString.Builder as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Demultiplex as I
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

-- | A low-level function to send an arbitrary frame over the wire.
sendFrame :: I.Frame -> I.WebSockets ()
sendFrame = I.send E.frame

-- | Receive a message
receiveMessage :: I.WebSockets (Maybe I.Message)
receiveMessage = I.WebSockets $ do
    mf <- I.unWebSockets receiveFrame
    case mf of
        Nothing -> return Nothing
        Just f  -> do
            s <- get
            let (msg, s') = I.demultiplex s f
            put s'
            case msg of
                Nothing -> I.unWebSockets receiveMessage
                Just m  -> return (Just m)

-- | Receive an application message. Automatically respond to control messages.
receiveDataMessage :: I.WebSockets (Maybe I.DataMessage)
receiveDataMessage = do
    mm <- receiveMessage
    case mm of
        Nothing -> return Nothing
        Just (I.DataMessage am) -> return (Just am)
        Just (I.ControlMessage cm) -> case cm of
            I.Close _ -> return Nothing
            I.Pong _  -> receiveDataMessage
            I.Ping pl -> do
                I.send E.controlMessage (I.Pong pl)
                receiveDataMessage

-- | Interpret the next message as UTF-8 encoded data
receiveTextData :: I.WebSockets (Maybe TL.Text)
receiveTextData = fmap TL.decodeUtf8 <$> receiveBinaryData

-- | Receive the next message as binary data
receiveBinaryData :: I.WebSockets (Maybe BL.ByteString)
receiveBinaryData = do
    dm <- receiveDataMessage
    case dm of
        Nothing           -> return Nothing
        Just (I.Text x)   -> return (Just x)
        Just (I.Binary x) -> return (Just x)

-- | Send a 'ByteString' to the socket immediately.
-- sendByteStringData :: ByteString -> I.WebSockets ()
-- sendByteStringData = I.send E.byteStringData

-- | A higher-level variant of 'sendByteStringData' which does the encoding for
-- you.
-- sendTextData :: Text -> I.WebSockets ()
-- sendTextData = I.send E.textData
