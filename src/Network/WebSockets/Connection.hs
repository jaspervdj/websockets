--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Connection
    ( PendingConnection (..)
    , acceptRequest
    , rejectRequest

    , Connection (..)
    , receive
    , receiveDataMessage
    , receiveData
    , send
    , sendDataMessage
    , sendTextData
    , sendBinaryData
    , sendClose
    ) where


--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder    (Builder)
import qualified Blaze.ByteString.Builder    as Builder
import           Control.Exception           (throw)
import qualified Data.ByteString             as B
import           Data.List                   (find)
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
    { pendingRequest :: RequestHead
    -- ^ Useful for e.g. inspecting the request path.
    , pendingIn      :: InputStream B.ByteString
    , pendingOut     :: OutputStream Builder
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
        return Connection
            { connectionType     = ServerConnection
            , connectionProtocol = protocol
            , connectionIn       = msgIn
            , connectionOut      = msgOut
            }
  where
    request       = pendingRequest pc
    versionHeader = [("Sec-WebSocket-Version",
        B.intercalate ", " $ concatMap headerVersions protocols)]


--------------------------------------------------------------------------------
rejectRequest :: PendingConnection -> B.ByteString -> IO ()
rejectRequest pc message = sendResponse pc $ response400 [] message


--------------------------------------------------------------------------------
data Connection = Connection
    { connectionType     :: ConnectionType
    , connectionProtocol :: Protocol
    , connectionIn       :: InputStream Message
    , connectionOut      :: OutputStream Message
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
receiveDataMessage :: Connection -> IO DataMessage
receiveDataMessage conn = do
    msg <- receive conn
    case msg of
        DataMessage am    -> return am
        ControlMessage cm -> case cm of
            Close _   -> throw ConnectionClosed
            -- TODO: do some 'onPong' callback?
            Pong _    -> receiveDataMessage conn
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
send conn msg = Streams.write (Just msg) (connectionOut conn)


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
-- | Send a friendly close message
sendClose :: WebSocketsData a => Connection -> a -> IO ()
sendClose conn = send conn . ControlMessage . Close . toLazyByteString
