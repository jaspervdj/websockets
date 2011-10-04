-- | How do you use this library? Here's how:
--
-- * Get an enumerator/iteratee pair from your favorite web server, or use
--   'I.runServer' to set up a simple standalone server.
--
-- * Read the 'I.Request' using 'receiveRequest'. Inspect its path and the
--   perform the initial 'H.handshake'. This yields a 'I.Response' which you can
--   send back using 'sendResponse'. The WebSocket is now ready.
--
-- There are (informally) three ways in which you can use the library:
--
-- * The most simple case: You don't care about the internal representation of
--   the messages. In this case, use the 'I.WebSocketsData' typeclass:
--   'receiveData', 'sendTextData' and 'sendBinaryData' will be useful.
--
-- * You have some protocol, and it is well-specified in which cases the client
--   should send text messages, and in which cases the client should send binary
--   messages. In this case, you can use the 'receiveDataMessage' and
--   'sendDataMessage' methods.
--
-- * You need to write a more low-level server in which you have control over
--   control frames (e.g. ping/pong). In this case, you can use the
--   'receiveMessage' and 'sendMessage' methods.
--
-- In some cases, you want to escape from the 'I.WebSockets' monad and send data
-- to the websocket from different threads. To this end, the
-- 'I.getMessageSender' method is provided.
--
-- For a full example, see:
--
-- <http://github.com/jaspervdj/websockets/tree/master/example>
module Network.WebSockets
    ( 
      -- * WebSocket type
      I.WebSocketsOptions (..)
    , I.defaultWebSocketsOptions
    , I.WebSockets
    , I.runWebSockets
    , I.runWebSocketsWith

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
    , H.handshake

      -- * Receiving
    -- , receiveRequest
    , receiveFrame
    , receiveMessage
    , receiveDataMessage
    , receiveData

      -- * Sending
    , sendResponse
    , sendFrame
    , I.sendMessage
    , sendTextData
    , sendBinaryData

      -- * Asynchronous sending
    , I.getMessageSender
    , I.close
    , I.ping
    , I.pong
    , I.textData
    , I.binaryData
    ) where

import Control.Monad.State (put, get)
import Control.Monad.Trans (liftIO)

import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Demultiplex as I
import qualified Network.WebSockets.Encode as E
import qualified Network.WebSockets.Handshake as H
import qualified Network.WebSockets.Monad as I
import qualified Network.WebSockets.Protocol as I
import qualified Network.WebSockets.Socket as I
import qualified Network.WebSockets.Types as I

-- | Read a 'I.Request' from the socket. Blocks until one is received and
-- returns 'Nothing' if the socket has been closed.
{-
receiveRequest :: I.WebSockets (Maybe I.RequestHttpPart)
receiveRequest = I.receive D.request
-}
-- This doesn't work this way any more. As the Protocol first has to be
-- determined by the request, we can't provide this as a WebSockets action. See
-- the various flavours of runWebSockets.

-- | Read a 'I.Frame' from the socket. Blocks until a frame is received and
-- returns 'Nothing' if the socket has been closed.
--
-- Note that a typical library user will want to use something like
-- 'receiveByteStringData' instead.
receiveFrame :: I.WebSockets (Maybe I.Frame)
receiveFrame = do
    proto <- I.getProtocol
    I.receive $ I.decodeFrame proto

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
            I.Pong _  -> do
                options <- I.getOptions
                liftIO $ I.onPong options
                receiveDataMessage
            I.Ping pl -> do
                I.sendMessage $ I.pong pl
                receiveDataMessage

-- | Receive a message, treating it as data transparently
receiveData :: I.WebSocketsData a => I.WebSockets (Maybe a)
receiveData = do
    dm <- receiveDataMessage
    case dm of
        Nothing           -> return Nothing
        Just (I.Text x)   -> return (Just $ I.fromLazyByteString x)
        Just (I.Binary x) -> return (Just $ I.fromLazyByteString x)

-- | Send a 'I.Response' to the socket immediately.
sendResponse :: I.Response -> I.WebSockets ()
sendResponse response = do
    sender <- I.getSender E.response
    liftIO $ sender response

-- | A low-level function to send an arbitrary frame over the wire.
sendFrame :: I.Frame -> I.WebSockets ()
sendFrame frame = do
    proto <- I.getProtocol
    sender <- I.getSender (I.encodeFrame proto)
    liftIO $ sender frame

-- | Send a text message
sendTextData :: I.WebSocketsData a => a -> I.WebSockets ()
sendTextData = I.sendMessage . I.textData

-- | Send some binary data
sendBinaryData :: I.WebSocketsData a => a -> I.WebSockets ()
sendBinaryData = I.sendMessage . I.binaryData
