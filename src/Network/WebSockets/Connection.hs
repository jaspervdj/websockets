--------------------------------------------------------------------------------
-- | This module exposes connection internals and should only be used if you
-- really know what you are doing.
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Connection
    ( PendingConnection (..)
    , acceptRequest
    , AcceptRequest(..)
    , defaultAcceptRequest
    , acceptRequestWith
    , rejectRequest
    , RejectRequest(..)
    , defaultRejectRequest
    , rejectRequestWith

    , Connection (..)

    , ConnectionOptions (..)
    , defaultConnectionOptions
    , defaultConnectionOptionsDeflate

    , receive
    , receiveDataMessage
    , receiveData
    , send
    , sendDataMessage
    , sendDataMessages
    , sendTextData
    , sendTextDatas
    , sendBinaryData
    , sendBinaryDatas
    , sendClose
    , sendCloseCode
    , sendPing

    , forkPingThread
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder    as Builder
import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Exception           (AsyncException, fromException,
                                              handle, throwIO)
import           Control.Monad               (unless, when)
import qualified Data.ByteString             as B
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import           Data.List                   (find)
import qualified Data.Text                   as T
import           Data.Word                   (Word16)


--------------------------------------------------------------------------------
import           Network.WebSockets.Http
import           Network.WebSockets.Protocol
import           Network.WebSockets.Stream   (Stream)
import qualified Network.WebSockets.Stream   as Stream
import           Network.WebSockets.Types

import           Network.WebSockets.Extensions.PermessageDeflate

--------------------------------------------------------------------------------
-- | A new client connected to the server. We haven't accepted the connection
-- yet, though.
data PendingConnection = PendingConnection
    { pendingOptions  :: !ConnectionOptions
    -- ^ Options, passed as-is to the 'Connection'
    , pendingRequest  :: !RequestHead
    -- ^ Useful for e.g. inspecting the request path.
    , pendingOnAccept :: !(Connection -> IO ())
    -- ^ One-shot callback fired when a connection is accepted, i.e., *after*
    -- the accepting response is sent to the client.
    , pendingStream   :: !Stream
    -- ^ Input/output stream
    }


--------------------------------------------------------------------------------
-- | This datatype allows you to set options for 'acceptRequestWith'.  It is
-- strongly recommended to use 'defaultAcceptRequest' and then modify the
-- various fields, that way new fields introduced in the library do not break
-- your code.
data AcceptRequest = AcceptRequest
    { acceptSubprotocol :: !(Maybe B.ByteString)
    -- ^ The subprotocol to speak with the client.  If 'pendingSubprotcols' is
    -- non-empty, 'acceptSubprotocol' must be one of the subprotocols from the
    -- list.
    , acceptHeaders :: !Headers
    -- ^ Extra headers to send with the response.
    }


--------------------------------------------------------------------------------
defaultAcceptRequest :: AcceptRequest
defaultAcceptRequest = AcceptRequest Nothing []


--------------------------------------------------------------------------------
-- | Utility
sendResponse :: PendingConnection -> Response -> IO ()
sendResponse pc rsp = Stream.write (pendingStream pc)
    (Builder.toLazyByteString (encodeResponse rsp))


--------------------------------------------------------------------------------
-- | Accept a pending connection, turning it into a 'Connection'.
acceptRequest :: PendingConnection -> IO Connection
acceptRequest pc = acceptRequestWith pc defaultAcceptRequest


--------------------------------------------------------------------------------
-- | This function is like 'acceptRequest' but allows you to set custom options
-- using the 'AcceptRequest' datatype.
acceptRequestWith :: PendingConnection -> AcceptRequest -> IO Connection
acceptRequestWith pc ar = case find (flip compatible request) protocols of
    Nothing       -> do
        sendResponse pc $ response400 versionHeader ""
        throwIO NotSupported
    Just protocol ->
      case negotiateDeflate (getRequestSecWebSocketExtensions request) $ connectionPermessageDeflate (pendingOptions pc) of
        Left err -> rejectRequestWith pc defaultRejectRequest{rejectMessage = err} >> throwIO NotSupported
        Right (exH, negotiatedPerMessage) -> do
         let subproto = maybe [] (\p -> [("Sec-WebSocket-Protocol", p)]) $ acceptSubprotocol ar
             headers = subproto ++ acceptHeaders ar ++ exH
             response = finishRequest protocol request headers
         sendResponse pc response
         parse <- decodeMessages protocol (pendingStream pc)
         write <- encodeMessages protocol ServerConnection (pendingStream pc)
         inflate <- wsInflate negotiatedPerMessage
         deflate <- wsDeflate negotiatedPerMessage
         sentRef    <- newIORef False
         let connection = Connection
                 { connectionOptions   = (pendingOptions pc){connectionPermessageDeflate = negotiatedPerMessage}
                 , connectionType      = ServerConnection
                 , connectionProtocol  = protocol
                 , connectionParse     = parse
                 , connectionWrite     = write
                 , connectionSentClose = sentRef
                 , connectionDeflate   = deflate
                 , connectionInflate   = inflate
                 }

         pendingOnAccept pc connection
         return connection
  where
    request       = pendingRequest pc
    versionHeader = [("Sec-WebSocket-Version",
        B.intercalate ", " $ concatMap headerVersions protocols)]


--------------------------------------------------------------------------------
-- | Parameters that allow you to tweak how a request is rejected.  Please use
-- 'defaultRejectRequest' and modify fields using record syntax so your code
-- will not break when new fields are added.
data RejectRequest = RejectRequest
    { -- | The status code, 400 by default.
      rejectCode         :: !Int
    , -- | The message, "Bad Request" by default
      rejectMessage      :: !B.ByteString
    , -- | Extra headers to be sent with the response.
      rejectHeaders      :: Headers
    , -- | Reponse body of the rejection.
      rejectBody :: !B.ByteString
    }


--------------------------------------------------------------------------------
defaultRejectRequest :: RejectRequest
defaultRejectRequest = RejectRequest
    { rejectCode    = 400
    , rejectMessage = "Bad Request"
    , rejectHeaders = []
    , rejectBody    = ""
    }


--------------------------------------------------------------------------------
rejectRequestWith
    :: PendingConnection  -- ^ Connection to reject
    -> RejectRequest      -- ^ Params on how to reject the request
    -> IO ()
rejectRequestWith pc reject = sendResponse pc $ Response
    ResponseHead
        { responseCode    = rejectCode reject
        , responseMessage = rejectMessage reject
        , responseHeaders = rejectHeaders reject
        }
    (rejectBody reject)


--------------------------------------------------------------------------------
rejectRequest
    :: PendingConnection  -- ^ Connection to reject
    -> B.ByteString       -- ^ Rejection response body
    -> IO ()
rejectRequest pc body = rejectRequestWith pc
    defaultRejectRequest {rejectBody = body}


--------------------------------------------------------------------------------
data Connection = Connection
    { connectionOptions   :: !ConnectionOptions
    , connectionType      :: !ConnectionType
    , connectionProtocol  :: !Protocol
    , connectionParse     :: !(IO (Maybe Message))
    , connectionWrite     :: !([Message] -> IO ())
    , connectionSentClose :: !(IORef Bool)
    -- ^ According to the RFC, both the client and the server MUST send
    -- a close control message to each other.  Either party can initiate
    -- the first close message but then the other party must respond.  Finally,
    -- the server is in charge of closing the TCP connection.  This IORef tracks
    -- if we have sent a close message and are waiting for the peer to respond.
    -- needs to be updated to some more generic plugin framework
    , connectionDeflate   :: !(Message -> IO Message)
    , connectionInflate   :: !(Message -> IO Message)
    }


--------------------------------------------------------------------------------
-- | Set options for a 'Connection'.
data ConnectionOptions = ConnectionOptions
    { connectionOnPong :: !(IO ())
      -- ^ Whenever a 'pong' is received, this IO action is executed. It can be
      -- used to tickle connections or fire missiles.
    , connectionPermessageDeflate :: Maybe PermessageDeflate
      -- ^ Settings of permessage deflate extension.
      -- Nothing will disable it
    }


--------------------------------------------------------------------------------
defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { connectionOnPong = return ()
    , connectionPermessageDeflate = Just defaultPermessageDeflate
    }

defaultConnectionOptionsDeflate :: ConnectionOptions
defaultConnectionOptionsDeflate = ConnectionOptions
    { connectionOnPong = return ()
    , connectionPermessageDeflate = Just defaultPermessageDeflate
    }

--------------------------------------------------------------------------------
receive :: Connection -> IO Message
receive conn = do
    mbMsg <- connectionParse conn
    case mbMsg of
        Nothing  -> throwIO ConnectionClosed
        Just msg -> connectionInflate conn msg


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
                throwIO $ CloseRequest i closeMsg
            Pong _    -> do
                connectionOnPong (connectionOptions conn)
                receiveDataMessage conn
            Ping pl   -> do
                send conn (ControlMessage (Pong pl))
                receiveDataMessage conn
        a -> print a >> error ""
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
send conn = sendAll conn . return

--------------------------------------------------------------------------------
sendAll :: Connection -> [Message] -> IO ()
sendAll conn msgs = do
    when (any isCloseMessage msgs) $
      writeIORef (connectionSentClose conn) True
    connectionWrite conn =<< mapM (connectionDeflate conn) msgs
  where
    isCloseMessage (ControlMessage (Close _ _)) = True
    isCloseMessage _                            = False

--------------------------------------------------------------------------------
-- | Send a 'DataMessage'
sendDataMessage :: Connection -> DataMessage -> IO ()
sendDataMessage conn = sendDataMessages conn . return

--------------------------------------------------------------------------------
-- | Send a collection of 'DataMessage's
sendDataMessages :: Connection -> [DataMessage] -> IO ()
sendDataMessages conn = sendAll conn . map DataMessage

--------------------------------------------------------------------------------
-- | Send a message as text
sendTextData :: WebSocketsData a => Connection -> a -> IO ()
sendTextData conn = sendTextDatas conn . return

--------------------------------------------------------------------------------
-- | Send a collection of messages as text
sendTextDatas :: WebSocketsData a => Connection -> [a] -> IO ()
sendTextDatas conn = sendDataMessages conn . map (Text . toLazyByteString)

--------------------------------------------------------------------------------
-- | Send a message as binary data
sendBinaryData :: WebSocketsData a => Connection -> a -> IO ()
sendBinaryData conn = sendBinaryDatas conn . return

--------------------------------------------------------------------------------
-- | Send a collection of messages as binary data
sendBinaryDatas :: WebSocketsData a => Connection -> [a] -> IO ()
sendBinaryDatas conn = sendDataMessages conn . map (Binary . toLazyByteString)

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
-- | Send a friendly close message and close code.  Similar to 'sendClose',
-- you should continue calling 'receiveDataMessage' until you receive a
-- 'CloseRequest' exception.
--
-- See <http://tools.ietf.org/html/rfc6455#section-7.4> for a list of close
-- codes.
sendCloseCode :: WebSocketsData a => Connection -> Word16 -> a -> IO ()
sendCloseCode conn code =
    send conn . ControlMessage . Close code . toLazyByteString


--------------------------------------------------------------------------------
-- | Send a ping
sendPing :: WebSocketsData a => Connection -> a -> IO ()
sendPing conn = send conn . ControlMessage . Ping . toLazyByteString


--------------------------------------------------------------------------------
-- | Forks a ping thread, sending a ping message every @n@ seconds over the
-- connection. The thread dies silently if the connection crashes or is closed.
forkPingThread :: Connection -> Int -> IO ()
forkPingThread conn n
    | n <= 0    = return ()
    | otherwise = do
        _ <- forkIO (ignore `handle` go 1)
        return ()
  where
    go :: Int -> IO ()
    go i = do
        threadDelay (n * 1000 * 1000)
        sendPing conn (T.pack $ show i)
        go (i + 1)

    ignore e = case fromException e of
        Just async -> throwIO (async :: AsyncException)
        Nothing    -> return ()
