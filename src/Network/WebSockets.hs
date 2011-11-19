-- | How do you use this library? Here's how:
--
-- Get an enumerator/iteratee pair from your favorite web server (or use a
-- library which provides integration). Alternatively, use 'I.runServer' to
-- set up a simple standalone server.
--
-- An application typically has the form of @I.Request -> I.WebSockets p ()@.
-- The first thing to do is accept or reject the request, usually based upon
-- the path in the 'I.Request'. An example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.WebSockets
-- >
-- > app :: Protocol p => Request -> WebSockets p ()
-- > app rq = case requestPath rq of
-- >    "/forbidden" -> rejectRequest rq "Forbidden!"
-- >    _            -> do
-- >        acceptRequest rq
-- >        ... actual application ...
--
-- You can now start using the socket for sending and receiving data. But what's
-- with the @p@ in @WebSockets p ()@?
--
-- Well, the answer is that this library aims to support many versions of the
-- WebSockets protocol. Unfortunately, not all versions of the protocol have the
-- same capabilities: for example, older versions are not able to send binary
-- data.
--
-- The library user (you!) choose which capabilities you need. Then, the browser
-- and library will negotiate at runtime which version will be actually used.
--
-- As an example, here are two applications which need different capabilities:
--
-- > import Network.WebSockets
-- > import qualified Data.ByteString as B
-- > import qualified Data.Text as T
-- > 
-- > app1 :: TextProtocol p => WebSockets p ()
-- > app1 = sendTextData (T.pack "Hello world!")
-- > 
-- > app2 :: BinaryProtocol p => WebSockets p ()
-- > app2 = sendBinaryData (B.pack [0 .. 100])
--
-- When you /tie the knot/, you will need to decide what protocol to use, to
-- prevent ambiguousness. A good rule of thumb is to select the lowest protocol
-- possible, since higher versions are generally backwards compatible in terms
-- of features. . For example, the following application uses only
-- /features from Hybi00/, and is therefore /compatible with Hybi10/ and later
-- protocols.
-- 
-- > app :: Request -> WebSockets Hybi00 ()
-- > app _ = app1
-- > 
-- > main :: IO ()
-- > main = runServer "0.0.0.0" 8000 app
-- 
-- In some cases, you want to escape from the 'I.WebSockets' monad and send data
-- to the websocket from different threads. To this end, the 'I.getSink' method
-- is provided. The next example spawns a thread which continuously spams the
-- client in another thread:
--
-- > import Control.Concurrent (forkIO)
-- > import Control.Monad (forever)
-- > import Control.Monad.Trans (liftIO)
-- > import Network.WebSockets
-- > import qualified Data.Text as T
-- > 
-- > spam :: TextProtocol p => WebSockets p ()
-- > spam = do
-- >     sink <- getSink
-- >     _ <- liftIO $ forkIO $ forever $
-- >         sendSink sink $ textData (T.pack "SPAM SPAM SPAM!")
-- >     sendTextData (T.pack "Hello world!")
--
-- For safety reasons, you can only read from the socket in the 'I.WebSockets'
-- monad.
--
-- For a full example, see:
--
-- <http://jaspervdj.be/websockets/example.html>
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
    , I.Hybi17

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
    , receiveFrame
    , receive
    , receiveDataMessage
    , receiveData

      -- * Sending
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
import qualified Network.WebSockets.Handshake.Http as I
import qualified Network.WebSockets.Monad as I
import qualified Network.WebSockets.Protocol as I
import qualified Network.WebSockets.Protocol.Hybi00 as I
import qualified Network.WebSockets.Protocol.Hybi10 as I
import qualified Network.WebSockets.Protocol.Hybi17 as I
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
receiveData :: (I.Protocol p, I.WebSocketsData a) => I.WebSockets p a
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
