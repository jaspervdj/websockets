module Network.WebSockets
    ( 
      -- * WebSocket type
      I.WebSockets
    , I.runWebSockets
    , I.runWithSocket

      -- * Types
    , I.Headers
    , I.Request (..)
    , I.Response (..)
    , I.Frame (..)

      -- * Initial handshake
    , receiveRequest
    , sendResponse
    , H.handshake

      -- * Sending and receiving
    , receiveFrame
    , receiveByteStringData
    , receiveTextData
    , I.send
    , sendByteStringData
    , sendTextData

      -- * Advanced sending
      -- TODO: getDataSender?
    , E.Encoder
    , I.Sender
    , I.getSender
    , E.response
    , E.byteStringData
    , E.textData
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Encode as E
import qualified Network.WebSockets.Handshake as H
import qualified Network.WebSockets.Monad as I
import qualified Network.WebSockets.Socket as I
import qualified Network.WebSockets.Types as I

receiveRequest :: I.WebSockets (Maybe I.Request)
receiveRequest = I.receive D.request

sendResponse :: I.Response -> I.WebSockets ()
sendResponse = I.send E.response

receiveFrame :: I.WebSockets (Maybe I.Frame)
receiveFrame = I.receive D.frame

receiveByteStringData :: I.WebSockets (Maybe ByteString)
receiveByteStringData = do
    frame <- receiveFrame
    case frame of
        Nothing         -> return Nothing
        Just (I.Data x) -> return (Just x)
        Just I.Close    -> return Nothing
        -- TODO: send pong & recurse
        Just I.Ping     -> error "TODO"
        Just I.Pong     -> error "TODO"

receiveTextData :: I.WebSockets (Maybe Text)
receiveTextData = (fmap . fmap) TE.decodeUtf8 receiveByteStringData

sendByteStringData :: ByteString -> I.WebSockets ()
sendByteStringData = I.send E.byteStringData

sendTextData :: Text -> I.WebSockets ()
sendTextData = I.send E.textData
