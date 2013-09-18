--------------------------------------------------------------------------------
-- | How do you use this library? Here's how:
--
-- Get an enumerator/iteratee pair from your favorite web server (or use a
-- library which provides integration). Alternatively, use 'I.runServer' to
-- set up a simple standalone server.
--
-- An application typically has the form of @I.Request -> I.WebSockets p ()@.
-- The first thing to do is accept or reject the request, usually based upon
-- the path in the 'I.Request'. An example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network.WebSockets
-- >
-- > app :: Protocol p => Request -> WebSockets p ()
-- > app rq = case requestPath rq of
-- >    "/forbidden" -> rejectRequest rq "Forbidden!"
-- >    _            -> do
-- >        acceptRequest rq
-- >        ... actual application ...
--
-- You can now start using the socket for sending and receiving data. But what's
-- with the @p@ in @WebSockets p ()@?
--
-- Well, the answer is that this library aims to support many versions of the
-- WebSockets protocol. Unfortunately, not all versions of the protocol have the
-- same capabilities: for example, older versions are not able to send binary
-- data.
--
-- The library user (you!) choose which capabilities you need. Then, the browser
-- and library will negotiate at runtime which version will be actually used.
--
-- As an example, here are two applications which need different capabilities:
--
-- > import Network.WebSockets
-- > import qualified Data.ByteString as B
-- > import qualified Data.Text as T
-- >
-- > app1 :: TextProtocol p => WebSockets p ()
-- > app1 = sendTextData (T.pack "Hello world!")
-- >
-- > app2 :: BinaryProtocol p => WebSockets p ()
-- > app2 = sendBinaryData (B.pack [0 .. 100])
--
-- When you /tie the knot/, you will need to decide what protocol to use, to
-- prevent ambiguousness. A good rule of thumb is to select the lowest protocol
-- possible, since higher versions are generally backwards compatible in terms
-- of features. . For example, the following application uses only
-- /features from Hybi00/, and is therefore /compatible with Hybi13/ and later
-- protocols.
--
-- > app :: Request -> WebSockets Hybi00 ()
-- > app _ = app1
-- >
-- > main :: IO ()
-- > main = runServer "0.0.0.0" 8000 app
--
-- In some cases, you want to escape from the 'I.WebSockets' monad and send data
-- to the websocket from different threads. To this end, the 'I.getSink' method
-- is provided. The next example spawns a thread which continuously spams the
-- client in another thread:
--
-- > import Control.Concurrent (forkIO)
-- > import Control.Monad (forever)
-- > import Control.Monad.Trans (liftIO)
-- > import Network.WebSockets
-- > import qualified Data.Text as T
-- >
-- > spam :: TextProtocol p => WebSockets p ()
-- > spam = do
-- >     sink <- getSink
-- >     _ <- liftIO $ forkIO $ forever $
-- >         sendSink sink $ textData (T.pack "SPAM SPAM SPAM!")
-- >     sendTextData (T.pack "Hello world!")
--
-- For safety reasons, you can only read from the socket in the 'I.WebSockets'
-- monad.
--
-- For a full example, see:
--
-- <http://jaspervdj.be/websockets/example.html>
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
    ) where


--------------------------------------------------------------------------------
import           Network.WebSockets.Client
import           Network.WebSockets.Connection
import           Network.WebSockets.Http
import           Network.WebSockets.Server
import           Network.WebSockets.Types
