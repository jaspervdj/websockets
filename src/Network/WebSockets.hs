--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets
    ( -- * Incoming connections and handshaking
      PendingConnection
    , pendingRequest
    , acceptRequest
    , rejectRequest

      -- * Sending and receiving messages
    , Connection
    , receive
    , receiveDataMessage
    , receiveData
    , send
    , sendDataMessage
    , sendTextData
    , sendBinaryData
    , sendClose

      -- * HTTP Types
    , Headers
    , Request (..)
    , RequestHead (..)
    , Response (..)
    , ResponseHead (..)

      -- * WebSocket message types
    , Message (..)
    , ControlMessage (..)
    , DataMessage (..)
    , WebSocketsData (..)

      -- * Exceptions
    , HandshakeException (..)
    , ConnectionException (..)


      -- * Running a standalone server
    , ServerApp
    , runServer

      -- * Running a client
    , ClientApp
    , runClient
    , runClientWith
    , runClientWithSocket
    , runClientWithStream

      -- * Closing a connection
    , close
    ) where


--------------------------------------------------------------------------------
import           Network.WebSockets.Client
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Server
import           Network.WebSockets.Types
