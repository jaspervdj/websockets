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
    , receiveData
    , sendFrame
    , sendData

      -- * Advanced sending
      -- TODO: getDataSender?
    , E.Encoder
    , I.Sender
    , I.getSender
    , E.response
    , E.frame
    ) where

import Data.ByteString (ByteString)

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

receiveData :: I.WebSockets (Maybe ByteString)
receiveData = do
    frame <- receiveFrame
    case frame of
        Just (I.Data x) -> return (Just x)
        _               -> error "TODO"

sendFrame :: I.Frame -> I.WebSockets ()
sendFrame = I.send E.frame

sendData :: ByteString -> I.WebSockets ()
sendData bs = sendFrame (I.Data bs)
