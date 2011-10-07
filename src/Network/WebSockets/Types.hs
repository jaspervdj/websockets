
{-# LANGUAGE DeriveDataTypeable #-}

-- | Primary types
module Network.WebSockets.Types
    ( Headers
    , Request (..)
    , Response (..)
    , FrameType (..)
    , Frame (..)
    , Message (..)
    , ControlMessage (..)
    , DataMessage (..)
    , WebSocketsData (..)
    , close
    , ping
    , pong
    , textData
    , binaryData

    , HandshakeError(..)
    , ConnectionError(..)
    , WsUserError(..)
    , Protocol(..)
    , RequestHttpPart(..)
    , Encoder, Decoder
    , criticalMissingFeatures
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

-- for {En,De}coder
import qualified Blaze.ByteString.Builder as B
import Data.Attoparsec (Parser)
import Network.WebSockets.Mask

-- just for providing the instance below.
import Control.Monad.Error (Error(..))

import qualified Data.Attoparsec.Enumerator as AE

import Network.WebSockets.Feature (Feature, Features)

import Data.Function

import Control.Exception (Exception(..), throw)
import Data.Typeable (Typeable)

-- | Request headers
type Headers = [(CI.CI B.ByteString, B.ByteString)]

-- | An alias so we don't have to import attoparsec everywhere
type Decoder a = Parser a

-- todo: restructure the module hierarchy

-- | The inverse of a parser
type Encoder a = Mask -> a -> B.Builder

data Protocol = Protocol
  { version       :: String        -- ^ Unique identifier for us.
  , headerVersion :: B.ByteString  -- ^ version as used in the "Sec-WebSocket-Version " header.
                                   -- this is usually not the same, or derivable from "version",
                                   -- e.g. for hybi10, it's "8".
  , encodeFrame   :: Encoder Frame
  , decodeFrame   :: Decoder Frame
  , finishRequest :: RequestHttpPart -> Decoder (Either HandshakeError Request)
  -- ^ Parse and validate the rest of the request. For hybi10, this is just
  -- validation, but hybi00 also needs to fetch a "security token"
  --
  -- Todo: Maybe we should introduce our own simplified error type here. (to be
  -- amended with the RequestHttpPart for the user)
  , features      :: Features
  }

instance Eq Protocol where
    (==) = (==) `on` version  -- yeah, right, a unique identifier!
instance Ord Protocol where
    compare = compare `on` version
    -- /not/ necessarily an ordering by date of publication!
instance Show Protocol where
    show = version

-- | Error in case of failed handshake. Will be thrown as an iteratee
-- exception. ('Error' condition).
data HandshakeError =
      NotSupported                             -- ^ We don't have a match for the protocol requested by the client.
                                               -- todo: version parameter
    | MalformedRequest RequestHttpPart String  -- ^ The request was somehow invalid (missing headers or wrong security token)
    | RequestRejected  Request String          -- ^ The request was well-formed, but the library user rejected it.
                                               -- (e.g. "unknown path")
    | OtherHandshakeError String               -- ^ for example "EOF came too early" (which is actually a parse error)
                                               -- or for your own errors. (like "unknown path"?)
    | MissingFeatures Features                 -- ^ The request was rejected because we require a feature the
                                               -- requested protocol doesn't support.
                                               -- todo: version parameter
    deriving (Show, Typeable)

-- | The connection couldn't be established or broke down unexpectedly. thrown
-- as an iteratee exception.
data ConnectionError =
      ParseError AE.ParseError       -- ^ The client sent malformed data.
    | ConnectionClosed               -- ^ the client closed the connection while
                                     -- we were trying to receive some data.
                                     --
                                     -- todo: Also want this for sending.
    deriving (Show, Typeable)

-- | /You/ did something wrong. E.g. not checking the required features before
-- using them. Thrown as a normal \"imprecise exception\" (and /not/ as an
-- iteratee 'Error')
data WsUserError =
      UncheckedMissingFeatures Features String  -- ^ missing features, protocol
    | OtherWsUserError String
    deriving (Show, Typeable)

instance Error HandshakeError where
    strMsg = OtherHandshakeError

-- todo: required?
instance Error WsUserError where
    strMsg = OtherWsUserError

instance Exception HandshakeError
instance Exception ConnectionError
instance Exception WsUserError

-- | (internal) Throw, as an exception (!), a UncheckedMissingFeatures. Used by
-- e.g. hybi00 when we try to send a control frame.
criticalMissingFeatures :: Protocol -> Features -> a
criticalMissingFeatures p fs =
    throw $ UncheckedMissingFeatures fs (version p)

-- | (internal) HTTP headers and requested path.
data RequestHttpPart = RequestHttpPart
    { requestHttpPath    :: !B.ByteString
    , requestHttpHeaders :: Headers
    } deriving (Eq, Show)

-- | Simple request type
data Request = Request
    { requestPath     :: !B.ByteString
    , requestHeaders  :: Headers
    , requestResponse :: Response
    }
    deriving (Show)

-- | Response to a 'Request'
data Response = Response
    { responseCode    :: !Int
    , responseMessage :: !B.ByteString
    , responseHeaders :: Headers
    , responseBody    :: B.ByteString
    } deriving (Show)

-- | A frame
data Frame = Frame
    { frameFin     :: !Bool
    , frameType    :: !FrameType
    , framePayload :: !BL.ByteString
    } deriving (Eq, Show)

-- | Type of a frame
data FrameType
    = ContinuationFrame
    | TextFrame
    | BinaryFrame
    | CloseFrame
    | PingFrame
    | PongFrame
    deriving (Eq, Show)

-- | The kind of message a server application typically deals with
data Message
    = ControlMessage ControlMessage
    | DataMessage DataMessage
    deriving (Show)

-- | Different control messages
data ControlMessage
    = Close BL.ByteString
    | Ping BL.ByteString
    | Pong BL.ByteString
    deriving (Show)

-- | For an end-user of this library, dealing with 'Frame's would be a bit
-- low-level. This is why define another type on top of it, which represents
-- data for the application layer.
data DataMessage
    = Text BL.ByteString
    | Binary BL.ByteString
    deriving (Show)

-- | In order to have an even more high-level API, we define a typeclass for
-- values the user can receive from and send to the socket. A few warnings
-- apply:
--
-- * Natively, everything is represented as a 'BL.ByteString', so this is the
--   fastest instance
--
-- * You should only use the 'TL.Text' or the 'T.Text' instance when you are
--   sure that the data is UTF-8 encoded (which is the case for 'Text'
--   messages).
--
-- * Messages can be very large. If this is the case, it might be inefficient to
--   use the strict 'B.ByteString' and 'T.Text' instances.
class WebSocketsData a where
    fromLazyByteString :: BL.ByteString -> a
    toLazyByteString   :: a -> BL.ByteString

instance WebSocketsData BL.ByteString where
    fromLazyByteString = id
    toLazyByteString   = id

instance WebSocketsData B.ByteString where
    fromLazyByteString = B.concat . BL.toChunks
    toLazyByteString   = BL.fromChunks . return

instance WebSocketsData TL.Text where
    fromLazyByteString = TL.decodeUtf8
    toLazyByteString   = TL.encodeUtf8

instance WebSocketsData T.Text where
    fromLazyByteString = T.concat . TL.toChunks . fromLazyByteString
    toLazyByteString   = toLazyByteString . TL.fromChunks . return

-- | Construct a close message
close :: WebSocketsData a => a -> Message
close = ControlMessage . Close . toLazyByteString

-- | Construct a ping message
ping :: WebSocketsData a => a -> Message
ping = ControlMessage . Ping . toLazyByteString

-- | Construct a pong message
pong :: WebSocketsData a => a -> Message
pong = ControlMessage . Pong . toLazyByteString

-- | Construct a text message
textData :: WebSocketsData a => a -> Message
textData = DataMessage . Text . toLazyByteString

-- | Construct a binary message
binaryData :: WebSocketsData a => a -> Message
binaryData = DataMessage . Binary . toLazyByteString

