--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets
    ( -- * Incoming connections and handshaking
      PendingConnection
    , pendingRequest
    , AcceptRequest(..)
    , acceptRequest
    , acceptRequestWith
    , rejectRequest

      -- * Main connection type
    , Connection

      -- * Options for connections
    , ConnectionOptions (..)
    , defaultConnectionOptions

      -- * Sending and receiving messages
    , receive
    , receiveDataMessage
    , receiveData
    , send
    , sendDataMessage
    , sendTextData
    , sendTextDatas
    , sendBinaryData
    , sendClose
    , sendPing

      -- * HTTP Types
    , Headers
    , Request (..)
    , RequestHead (..)
    , getRequestSubprotocols
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
    , runServerWith

      -- * Utilities for writing your own server
    , makeListenSocket
    , makePendingConnection
    , makePendingConnectionFromStream

      -- * Running a client
    , ClientApp
    , runClient
    , runClientWith
    , runClientWithSocket
    , runClientWithStream

      -- * Utilities
    , forkPingThread
    ) where


--------------------------------------------------------------------------------
import           Network.WebSockets.Client
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Server
import           Network.WebSockets.Types
