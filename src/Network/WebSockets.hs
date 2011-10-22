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
--   'receive' and 'send' methods.
--
-- In some cases, you want to escape from the 'I.WebSockets' monad and send data
-- to the websocket from different threads. To this end, the
-- 'I.getMessageSender' method is provided.
--
-- For a full example, see:
--
-- <http://github.com/jaspervdj/websockets/tree/master/example>
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets
    ( 
      -- * WebSocket type
      I.WebSocketsOptions (..)
    , I.defaultWebSocketsOptions
    , I.WebSockets
    , I.runWebSockets
    , I.runWebSocketsWith
    , I.runWebSocketsHandshake
    , I.runWebSocketsWithHandshake

      -- * Protocol versions
    , I.Protocol
    , I.TextProtocol
    , I.BinaryProtocol
    , I.Hybi00
    , I.Hybi10

      -- * A simple standalone server
    , I.runServer
    , I.runWithSocket

      -- * Types
    , I.Headers
    , I.RequestHttpPart (..)
    , I.Request (..)
    , I.Response (..)
    , I.FrameType (..)
    , I.Frame (..)
    , I.Message (..)
    , I.ControlMessage (..)
    , I.DataMessage (..)
    , I.WebSocketsData (..)

      -- * Handshake
    , acceptRequest
    , rejectRequest

      -- * Various
    , I.getVersion

      -- * Receiving
    -- , receiveRequest
    , receiveFrame
    , receive
    , receiveDataMessage
    , receiveData

      -- * Sending
    , sendResponse
    , sendFrame
    , I.send
    , sendTextData
    , sendBinaryData

      -- * Asynchronous sending
    , I.Sink
    , I.sendSink
    , I.getSink
    , I.close
    , I.ping
    , I.pong
    , I.textData
    , I.binaryData
    , I.spawnPingThread

      -- * Error Handling
    , I.throwWsError
    , I.catchWsError
    , I.HandshakeError(..)
    , I.ConnectionError(..)
    ) where

import Control.Monad.State (put, get)
import Control.Monad.Trans (liftIO)

import qualified Network.WebSockets.Demultiplex as I
import qualified Network.WebSockets.Handshake as I
import qualified Network.WebSockets.Http as I
import qualified Network.WebSockets.Monad as I
import qualified Network.WebSockets.Protocol as I
import qualified Network.WebSockets.Protocol.Hybi00 as I
import qualified Network.WebSockets.Protocol.Hybi10 as I
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe
import qualified Network.WebSockets.Socket as I
import qualified Network.WebSockets.Types as I

-- This doesn't work this way any more. As the Protocol first has to be
-- determined by the request, we can't provide this as a WebSockets action. See
-- the various flavours of runWebSockets.

-- | Read a 'I.Frame' from the socket. Blocks until a frame is received. If the
-- socket is closed, throws 'ConnectionClosed' (a 'ConnectionError')
--
-- Note that a typical library user will want to use something like
-- 'receiveByteStringData' instead.
receiveFrame :: I.Protocol p => I.WebSockets p I.Frame
receiveFrame = do
    proto <- I.getProtocol
    I.receiveWith $ I.decodeFrame proto

-- | Receive a message
receive :: I.Protocol p => I.WebSockets p (I.Message p)
receive = I.WebSockets $ do
    f <- I.unWebSockets receiveFrame
    s <- get
    let (msg, s') = I.demultiplex s f
    put s'
    case msg of
        Nothing -> I.unWebSockets receive
        Just m  -> return m

-- | Receive an application message. Automatically respond to control messages.
receiveDataMessage :: I.Protocol p => I.WebSockets p (I.DataMessage p)
receiveDataMessage = do
    m <- receive
    case m of
        (I.DataMessage am) -> return am
        (I.ControlMessage cm) -> case cm of
            I.Close _ -> I.throwWsError I.ConnectionClosed
            I.Pong _  -> do
                options <- I.getOptions
                liftIO $ I.onPong options
                receiveDataMessage
            I.Ping pl -> do
                -- Note that we are using an /unsafe/ pong here. If the 
                -- underlying protocol cannot encode this pong, our thread will
                -- crash. We assume, however that the protocol /is/ able to
                -- encode the pong, since it was able to encode a ping.
                I.send $ Unsafe.pong pl
                receiveDataMessage

-- | Receive a message, treating it as data transparently
receiveData :: I.Protocol p => I.WebSocketsData a => I.WebSockets p a
receiveData = do
    dm <- receiveDataMessage
    case dm of
        I.Text x   -> return (I.fromLazyByteString x)
        I.Binary x -> return (I.fromLazyByteString x)

-- | Send a 'I.Response' to the socket immediately.
sendResponse :: I.Protocol p => I.Response -> I.WebSockets p ()
sendResponse response = I.sendWith I.encodeResponse response

-- | A low-level function to send an arbitrary frame over the wire.
sendFrame :: I.Protocol p => I.Frame -> I.WebSockets p ()
sendFrame frame = do
    proto <- I.getProtocol
    I.sendWith (I.encodeFrame proto) frame

-- | Send a text message
sendTextData :: (I.TextProtocol p, I.WebSocketsData a) => a -> I.WebSockets p ()
sendTextData = I.send . I.textData

-- | Send some binary data
sendBinaryData :: (I.BinaryProtocol p, I.WebSocketsData a)
               => a -> I.WebSockets p ()
sendBinaryData = I.send . I.binaryData

-- | Reject a request, sending a 400 (Bad Request) to the client and throwing a
-- RequestRejected (HandshakeError)
rejectRequest :: I.Protocol p
              => I.Request -> String -> I.WebSockets p a
rejectRequest req reason = failHandshakeWith $ I.RequestRejected req reason

failHandshakeWith :: forall p a. I.Protocol p
                  => I.HandshakeError -> I.WebSockets p a
failHandshakeWith err = do
    sendResponse $ I.responseError (undefined :: p) err
    I.throwWsError err

-- | Accept a request. After this, you can start sending and receiving data.
acceptRequest :: I.Protocol p => I.Request -> I.WebSockets p ()
acceptRequest = sendResponse . I.requestResponse
