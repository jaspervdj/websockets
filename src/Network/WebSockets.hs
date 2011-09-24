-- | How do you use this library? Here's how:
--
-- * Get an enumerator/iteratee pair from your favorite web server, or use
--   'I.runServer' to set up a simple standalone server.
--
-- * Read the 'I.Request' using 'receiveRequest'. Inspect its path and the perform
--   the initial 'H.handshake'. This yields a 'I.Response' which you can send
--   back using 'sendResponse'. The WebSocket is now ready.
--
-- Now you're all set! The WebSockets protocol defines two types of messages:
-- binary, and UTF-8 encoded messages. At the Haskell side, you can write very
-- flexible clients using the 'I.WebSocketsData' typeclass. This allows you to
-- deal with binary and text data using the different Haskell string types
-- (bytestrings and text).
--
-- In order to receive messages, most users of this library will want to use
-- 'receiveData', which simply gives you the data messages in the format you
-- choose.
--
-- If you want to send messages, the message type must be known: so, use
-- either 'sendTextData' or 'sendBinaryData'.
--
-- In some cases, you want to escape from the 'I.WebSockets' monad and send data
-- to the websocket from different threads. To this end, the 'I.getSender'
-- method is provided.
--
-- For a full example, see:
--
-- <http://github.com/jaspervdj/websockets/tree/master/example>
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
    , I.WebSocketsData (..)

      -- * Initial handshake
    , H.HandshakeError (..)
    , receiveRequest
    , sendResponse
    , H.handshake

      -- * Sending and receiving
    , receiveFrame
    , sendFrame
    , receiveMessage
    , sendMessage
    , receiveDataMessage
    , sendDataMessage
    , receiveData
    , sendTextData
    , sendBinaryData

      -- * Advanced sending
    , E.Encoder
    , I.Sender
    , I.send
    , I.getSender
    , E.response
    , E.frame
    , E.message
    , E.controlMessage
    , E.dataMessage
    , E.textData
    , E.binaryData

    , I.sendIter
    , I.receiveEnum
    ) where

import Control.Monad.State (put, get)

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

-- | Send a message
sendMessage :: I.Message -> I.WebSockets ()
sendMessage = I.send E.message

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

-- | Send an application-level message.
sendDataMessage :: I.DataMessage -> I.WebSockets ()
sendDataMessage = I.send E.dataMessage

-- | Receive a message, treating it as data transparently
receiveData :: I.WebSocketsData a => I.WebSockets (Maybe a)
receiveData = do
    dm <- receiveDataMessage
    case dm of
        Nothing           -> return Nothing
        Just (I.Text x)   -> return (Just $ I.fromLazyByteString x)
        Just (I.Binary x) -> return (Just $ I.fromLazyByteString x)

-- | Send a text message
sendTextData :: I.WebSocketsData a => a -> I.WebSockets ()
sendTextData = I.send E.textData

-- | Send some binary data
sendBinaryData :: I.WebSocketsData a => a -> I.WebSockets ()
sendBinaryData = I.send E.binaryData
