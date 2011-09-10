module Network.WebSockets
    ( 
      -- * WebSocket type
      I.WebSocket
    , I.new
    , I.close

      -- * Types
    , I.Headers
    , I.Request
    , I.Response
    , I.Frame

      -- * Initial handshake
    , receiveRequest
    , sendResponse
    , H.handshake

      -- * Sending and receiving
    , receiveFrame
    , sendFrame
    ) where

import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Encode as E
import qualified Network.WebSockets.Handshake as H
import qualified Network.WebSockets.Types as I
import qualified Network.WebSockets.WebSocket as I

receiveRequest :: I.WebSocket -> IO (Maybe I.Request)
receiveRequest = I.receive D.request

sendResponse :: I.WebSocket -> I.Response -> IO ()
sendResponse = I.send E.response

receiveFrame :: I.WebSocket -> IO (Maybe I.Frame)
receiveFrame = I.receive D.frame

sendFrame :: I.WebSocket -> I.Frame -> IO ()
sendFrame = I.send E.frame
