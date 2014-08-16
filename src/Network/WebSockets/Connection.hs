--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Connection
    ( PendingConnection (..)
    , acceptRequest
    , rejectRequest

    , Connection (..)

    , ConnectionOptions (..)
    , defaultConnectionOptions

    , receive
    , receiveDataMessage
    , receiveData
    , send
    , sendDataMessage
    , sendTextData
    , sendBinaryData
    , sendClose
    , sendCloseCode
    , sendPing
    ) where


--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder    (Builder)
import qualified Blaze.ByteString.Builder    as Builder
import           Control.Exception           (throw)
import           Control.Monad               (unless)
import qualified Data.ByteString             as B
import           Data.List                   (find)
import           Data.IORef                  (IORef, newIORef, readIORef, writeIORef)
import           Data.Word                   (Word16)
import           System.IO.Streams           (InputStream, OutputStream)
import qualified System.IO.Streams           as Streams


--------------------------------------------------------------------------------
import           Network.WebSockets.Http
import           Network.WebSockets.Protocol
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
-- | A new client connected to the server. We haven't accepted the connection
-- yet, though.
data PendingConnection = PendingConnection
    { pendingOptions  :: ConnectionOptions
    -- ^ Options, passed as-is to the 'Connection'
    , pendingRequest  :: RequestHead
    -- ^ Useful for e.g. inspecting the request path.
    , pendingOnAccept :: Connection -> IO ()
    -- ^ One-shot callback fired when a connection is accepted, i.e., *after*
    -- the accepting response is sent to the client.
    , pendingIn       :: InputStream B.ByteString
    -- ^ Input stream
    , pendingOut      :: OutputStream Builder
    -- ^ Output stream
    }


--------------------------------------------------------------------------------
-- | Utility
sendResponse :: PendingConnection -> Response -> IO ()
sendResponse pc rsp = do
    Streams.write (Just (encodeResponse rsp)) (pendingOut pc)
    Streams.write (Just Builder.flush)        (pendingOut pc)


--------------------------------------------------------------------------------
acceptRequest :: PendingConnection -> IO Connection
acceptRequest pc = case find (flip compatible request) protocols of
    Nothing       -> do
        sendResponse pc $ response400 versionHeader ""
        throw NotSupported
    Just protocol -> do
        let response = finishRequest protocol request
        sendResponse pc response
        msgIn  <- decodeMessages protocol (pendingIn pc)
        msgOut <- encodeMessages protocol ServerConnection (pendingOut pc)
        sentRef <- newIORef False
        let connection = Connection
                { connectionOptions   = pendingOptions pc
                , connectionType      = ServerConnection
                , connectionProtocol  = protocol
                , connectionIn        = msgIn
                , connectionOut       = msgOut
                , connectionSentClose = sentRef
                }

        pendingOnAccept pc connection
        return connection
  where
    request       = pendingRequest pc
    versionHeader = [("Sec-WebSocket-Version",
        B.intercalate ", " $ concatMap headerVersions protocols)]


--------------------------------------------------------------------------------
rejectRequest :: PendingConnection -> B.ByteString -> IO ()
rejectRequest pc message = sendResponse pc $ response400 [] message


--------------------------------------------------------------------------------
data Connection = Connection
    { connectionOptions  :: ConnectionOptions
    , connectionType     :: ConnectionType
    , connectionProtocol :: Protocol
    , connectionIn       :: InputStream Message
    , connectionOut      :: OutputStream Message
    , connectionSentClose :: IORef Bool
    -- ^ According to the RFC, both the client and the server MUST send
    -- a close control message to each other.  Either party can initiate
    -- the first close message but then the other party must respond.  Finally,
    -- the server is in charge of closing the TCP connection.  This IORef tracks
    -- if we have sent a close message and are waiting for the peer to respond.
    }


--------------------------------------------------------------------------------
data ConnectionOptions = ConnectionOptions
    { connectionOnPong :: IO ()
    }


--------------------------------------------------------------------------------
defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { connectionOnPong = return ()
    }


--------------------------------------------------------------------------------
receive :: Connection -> IO Message
receive conn = do
    mmsg <- Streams.read (connectionIn conn)
    case mmsg of
        Nothing  -> throw ConnectionClosed
        Just msg -> return msg


--------------------------------------------------------------------------------
-- | Receive an application message. Automatically respond to control messages.
--
-- When the peer sends a close control message, an exception of type 'CloseRequest'
-- is thrown.  The peer can send a close control message either to initiate a
-- close or in response to a close message we have sent to the peer.  In either
-- case the 'CloseRequest' exception will be thrown.  The RFC specifies that
-- the server is responsible for closing the TCP connection, which should happen
-- after receiving the 'CloseRequest' exception from this function.
--
-- This will throw 'ConnectionClosed' if the TCP connection dies unexpectedly.
receiveDataMessage :: Connection -> IO DataMessage
receiveDataMessage conn = do
    msg <- receive conn
    case msg of
        DataMessage am    -> return am
        ControlMessage cm -> case cm of
            Close i closeMsg -> do
                hasSentClose <- readIORef $ connectionSentClose conn
                unless hasSentClose $ send conn msg
                throw $ CloseRequest i closeMsg
            Pong _    -> do
                connectionOnPong (connectionOptions conn)
                receiveDataMessage conn
            Ping pl   -> do
                send conn (ControlMessage (Pong pl))
                receiveDataMessage conn


--------------------------------------------------------------------------------
-- | Receive a message, converting it to whatever format is needed.
receiveData :: WebSocketsData a => Connection -> IO a
receiveData conn = do
    dm <- receiveDataMessage conn
    case dm of
        Text x   -> return (fromLazyByteString x)
        Binary x -> return (fromLazyByteString x)


--------------------------------------------------------------------------------
send :: Connection -> Message -> IO ()
send conn msg = do
    case msg of
        (ControlMessage (Close _ _)) -> writeIORef (connectionSentClose conn) True
        _ -> return ()
    Streams.write (Just msg) (connectionOut conn)


--------------------------------------------------------------------------------
-- | Send a 'DataMessage'
sendDataMessage :: Connection -> DataMessage -> IO ()
sendDataMessage conn = send conn . DataMessage


--------------------------------------------------------------------------------
-- | Send a message as text
sendTextData :: WebSocketsData a => Connection -> a -> IO ()
sendTextData conn = sendDataMessage conn . Text . toLazyByteString


--------------------------------------------------------------------------------
-- | Send a message as binary data
sendBinaryData :: WebSocketsData a => Connection -> a -> IO ()
sendBinaryData conn = sendDataMessage conn . Binary . toLazyByteString


--------------------------------------------------------------------------------
-- | Send a friendly close message.  Note that after sending this message,
-- you should still continue calling 'receiveDataMessage' to process any
-- in-flight messages.  The peer will eventually respond with a close control
-- message of its own which will cause 'receiveDataMessage' to throw the
-- 'CloseRequest' exception.  This exception is when you can finally consider
-- the connection closed.
sendClose :: WebSocketsData a => Connection -> a -> IO ()
sendClose conn = sendCloseCode conn 1000


--------------------------------------------------------------------------------
-- | Send a friendly close message and close code.  Similar to 'sendClose', you should
-- continue calling 'receiveDataMessage' until you receive a 'CloseRequest' exception.
-- See <http://tools.ietf.org/html/rfc6455#section-7.4> for a list of close
-- codes.
sendCloseCode :: WebSocketsData a => Connection -> Word16 -> a -> IO ()
sendCloseCode conn code = send conn . ControlMessage . Close code . toLazyByteString


--------------------------------------------------------------------------------
-- | Send a ping
sendPing :: WebSocketsData a => Connection -> a -> IO ()
sendPing conn = send conn . ControlMessage . Ping . toLazyByteString
