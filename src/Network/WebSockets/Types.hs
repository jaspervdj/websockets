
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

    , HandshakeError (..)
    , ConnectionError (..)
    , RequestHttpPart (..)
    , Encoder
    , Decoder
    ) where

import Control.Exception (Exception(..))
import Control.Monad.Error (Error(..))
import Data.Typeable (Typeable)
import qualified Data.Attoparsec.Enumerator as AE

import Data.Attoparsec (Parser)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.WebSockets.Mask

-- | Request headers
type Headers = [(CI.CI B.ByteString, B.ByteString)]

-- | An alias so we don't have to import attoparsec everywhere
type Decoder p a = Parser a

-- todo: restructure the module hierarchy

-- | The inverse of a parser
type Encoder p a = Mask -> a -> B.Builder

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

instance Error HandshakeError where
    strMsg = OtherHandshakeError

instance Exception HandshakeError
instance Exception ConnectionError

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
data Message p
    = ControlMessage (ControlMessage p)
    | DataMessage    (DataMessage p)
    deriving (Show)

-- | Different control messages
data ControlMessage p
    = Close BL.ByteString
    | Ping BL.ByteString
    | Pong BL.ByteString
    deriving (Show)

-- | For an end-user of this library, dealing with 'Frame's would be a bit
-- low-level. This is why define another type on top of it, which represents
-- data for the application layer.
data DataMessage p
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
close :: WebSocketsData a => a -> Message p
close = ControlMessage . Close . toLazyByteString

-- | Construct a ping message
ping :: WebSocketsData a => a -> Message p
ping = ControlMessage . Ping . toLazyByteString

-- | Construct a pong message
pong :: WebSocketsData a => a -> Message p
pong = ControlMessage . Pong . toLazyByteString

-- | Construct a text message
textData :: WebSocketsData a => a -> Message p
textData = DataMessage . Text . toLazyByteString

-- | Construct a binary message
binaryData :: WebSocketsData a => a -> Message p
binaryData = DataMessage . Binary . toLazyByteString
