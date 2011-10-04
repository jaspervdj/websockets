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
    , Protocol(..), RequestHttpPart(..)
    , Encoder, Decoder
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

-- | Request headers
type Headers = [(CI.CI B.ByteString, B.ByteString)]

-- | An alias so we don't have to import attoparsec everywhere
type Decoder a = Parser a

-- todo: restructure the module hierarchy

-- | The inverse of a parser
type Encoder a = Mask -> a -> B.Builder

data Protocol = Protocol
  { version :: String
  , encodeFrame :: Encoder Frame
  , decodeFrame :: Decoder Frame
  , finishRequest :: RequestHttpPart -> Decoder (Either HandshakeError Request)
  -- ^ Parse and validate the rest of the request. For hybi10, this is just
  -- validation, but hybi00 also needs to fetch a "security token"
  }

-- | Error in case of failed handshake.
data HandshakeError = HandshakeError String
                    deriving (Show)

-- | (internal) HTTP headers and requested path.
data RequestHttpPart = RequestHttpPart
    { requestHttpPath    :: !B.ByteString
    , requestHttpHeaders :: Headers
    } deriving (Show)

-- | Simple request type
data Request = Request
    { requestPath     :: !B.ByteString
    , requestHeaders  :: Headers
    , requestProtocol :: Protocol
    }

-- | Response to a 'Request'
data Response = Response
    { responseCode    :: !Int
    , responseMessage :: !B.ByteString
    , responseHeaders :: Headers
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
