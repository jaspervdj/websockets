--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Connection
    ( PendingConnection (..)
    , AcceptRequest(..)
    , acceptRequest
    , acceptRequestWith
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
    , sendPing
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
data AcceptRequest = AcceptRequest
    { acceptSubprotocol :: Maybe B.ByteString
    -- ^ The subprotocol to speak with the client.  If 'pendingSubprotcols' is
    -- non-empty, 'acceptSubprotocol' must be one of the subprotocols from the list.
    }


--------------------------------------------------------------------------------
-- | Utility
sendResponse :: PendingConnection -> Response -> IO ()
sendResponse pc rsp = do
    Streams.write (Just (encodeResponse rsp)) (pendingOut pc)
    Streams.write (Just Builder.flush)        (pendingOut pc)


--------------------------------------------------------------------------------
acceptRequest :: PendingConnection -> IO Connection
acceptRequest pc = acceptRequestWith pc $ AcceptRequest Nothing


--------------------------------------------------------------------------------
acceptRequestWith :: PendingConnection -> AcceptRequest -> IO Connection
acceptRequestWith pc ar = case find (flip compatible request) protocols of
    Nothing       -> do
        sendResponse pc $ response400 versionHeader ""
        throw NotSupported
    Just protocol -> do
        let subproto = maybe [] (\p -> [("Sec-WebSocket-Protocol", p)]) $ acceptSubprotocol ar
            response = finishRequest protocol request subproto
        sendResponse pc response
        msgIn  <- decodeMessages protocol (pendingIn pc)
        msgOut <- encodeMessages protocol ServerConnection (pendingOut pc)
        let connection = Connection
                { connectionOptions  = pendingOptions pc
                , connectionType     = ServerConnection
                , connectionProtocol = protocol
                , connectionIn       = msgIn
                , connectionOut      = msgOut
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
receiveDataMessage :: Connection -> IO DataMessage
receiveDataMessage conn = do
    msg <- receive conn
    case msg of
        DataMessage am    -> return am
        ControlMessage cm -> case cm of
            Close _   -> throw ConnectionClosed
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


--------------------------------------------------------------------------------
-- | Send a ping
sendPing :: WebSocketsData a => Connection -> a -> IO ()
sendPing conn = send conn . ControlMessage . Ping . toLazyByteString
