--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets
    ( -- * Incoming connections and handshaking
      PendingConnection
    , pendingRequest
    , acceptRequest
    , AcceptRequest(..)
    , defaultAcceptRequest
    , acceptRequestWith
    , rejectRequest
    , RejectRequest(..)
    , defaultRejectRequest
    , rejectRequestWith

      -- * Main connection type
    , Connection

      -- * Options for connections
    , ConnectionOptions (..)
    , defaultConnectionOptions

      -- ** Compression options
    , CompressionOptions (..)
    , PermessageDeflate (..)
    , defaultPermessageDeflate

      -- ** Protection limits
    , SizeLimit (..)

      -- * Sending and receiving messages
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
    , ServerOptions (..)
    , defaultServerOptions
    , runServerWithOptions

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
    , newClientConnection

      -- * Utilities
    , PingPongOptions(..)
    , defaultPingPongOptions
    , withPingPong
    , withPingThread
    , forkPingThread
    ) where


--------------------------------------------------------------------------------
import           Network.WebSockets.Client
import           Network.WebSockets.Connection
import           Network.WebSockets.Connection.PingPong
import           Network.WebSockets.Http
import           Network.WebSockets.Server
import           Network.WebSockets.Types
