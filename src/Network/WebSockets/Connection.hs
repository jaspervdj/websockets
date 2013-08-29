--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Connection
    ( PendingConnection (..)
    , acceptRequest
    , rejectRequest

    , Connection (..)
    , receive
    , send
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
        putStrLn "Sending response..."  -- DEBUG
        sendResponse pc response
        putStrLn "Response sent."  -- DEBUG
        msgIn  <- decodeMessages protocol (pendingIn pc)
        msgOut <- encodeMessages protocol (pendingOut pc)
        return Connection
            { connectionProtocol = protocol
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
    { connectionProtocol :: Protocol
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
send :: Connection -> Message -> IO ()
send conn msg = Streams.write (Just msg) (connectionOut conn)
