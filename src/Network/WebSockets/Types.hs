--------------------------------------------------------------------------------
-- | Primary types
{-# LANGUAGE DeriveDataTypeable #-}
module Network.WebSockets.Types
    ( Message (..)
    , ControlMessage (..)
    , DataMessage (..)
    , WebSocketsData (..)

    , HandshakeException (..)
    , ConnectionException (..)

    , ConnectionType (..)

    , decodeUtf8Lenient
    , decodeUtf8Strict
    ) where


--------------------------------------------------------------------------------
import           Control.Exception        (Exception (..))
import           Control.Exception        (throw, try)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import qualified Data.Text.Encoding.Error as TL
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Data.Typeable            (Typeable)
import           Data.Word                (Word16)
import           System.IO.Unsafe         (unsafePerformIO)


--------------------------------------------------------------------------------
import           Network.WebSockets.Http


--------------------------------------------------------------------------------
-- | The kind of message a server application typically deals with
data Message
    = ControlMessage ControlMessage
    -- | Reserved bits, actual message
    | DataMessage Bool Bool Bool DataMessage
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Different control messages
data ControlMessage
    = Close Word16 BL.ByteString
    | Ping BL.ByteString
    | Pong BL.ByteString
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | For an end-user of this library, dealing with 'Frame's would be a bit
-- low-level. This is why define another type on top of it, which represents
-- data for the application layer.
--
-- There are currently two kinds of data messages supported by the WebSockets
-- protocol:
--
-- * Textual UTF-8 encoded data.  This corresponds roughly to sending a String
-- in JavaScript.
--
-- * Binary data.  This corresponds roughly to send an ArrayBuffer in
-- JavaScript.
data DataMessage
    -- | A textual message.  The second field /might/ contain the decoded UTF-8
    -- text for caching reasons.  This field is computed lazily so if it's not
    -- accessed, it should have no performance impact.
    = Text BL.ByteString (Maybe TL.Text)
    -- | A binary message.
    | Binary BL.ByteString
    deriving (Eq, Show)


--------------------------------------------------------------------------------
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
    fromDataMessage :: DataMessage -> a

    fromLazyByteString :: BL.ByteString -> a
    toLazyByteString   :: a -> BL.ByteString


--------------------------------------------------------------------------------
instance WebSocketsData BL.ByteString where
    fromDataMessage (Text   bl _) = bl
    fromDataMessage (Binary bl)   = bl

    fromLazyByteString = id
    toLazyByteString   = id


--------------------------------------------------------------------------------
instance WebSocketsData B.ByteString where
    fromDataMessage (Text   bl _) = fromLazyByteString bl
    fromDataMessage (Binary bl)   = fromLazyByteString bl

    fromLazyByteString = B.concat . BL.toChunks
    toLazyByteString   = BL.fromChunks . return


--------------------------------------------------------------------------------
instance WebSocketsData TL.Text where
    fromDataMessage (Text   _  (Just tl)) = tl
    fromDataMessage (Text   bl Nothing)   = fromLazyByteString bl
    fromDataMessage (Binary bl)           = fromLazyByteString bl


    fromLazyByteString = TL.decodeUtf8
    toLazyByteString   = TL.encodeUtf8


--------------------------------------------------------------------------------
instance WebSocketsData T.Text where
    fromDataMessage (Text   _ (Just tl)) = T.concat (TL.toChunks tl)
    fromDataMessage (Text   bl Nothing)  = fromLazyByteString bl
    fromDataMessage (Binary bl)          = fromLazyByteString bl

    fromLazyByteString = T.concat . TL.toChunks . fromLazyByteString
    toLazyByteString   = toLazyByteString . TL.fromChunks . return


--------------------------------------------------------------------------------
-- | Various exceptions that can occur while receiving or transmitting messages
data ConnectionException
    -- | The peer has requested that the connection be closed, and included
    -- a close code and a reason for closing.  When receiving this exception,
    -- no more messages can be sent.  Also, the server is responsible for
    -- closing the TCP connection once this exception is received.
    --
    -- See <http://tools.ietf.org/html/rfc6455#section-7.4> for a list of close
    -- codes.
    = CloseRequest Word16 BL.ByteString

    -- | The peer unexpectedly closed the connection while we were trying to
    -- receive some data.  This is a violation of the websocket RFC since the
    -- TCP connection should only be closed after sending and receiving close
    -- control messages.
    | ConnectionClosed

    -- | The client sent garbage, i.e. we could not parse the WebSockets stream.
    | ParseException String

    -- | The client sent invalid UTF-8.  Note that this exception will only be
    -- thrown if strict decoding is set in the connection options.
    | UnicodeException String
    deriving (Eq, Show, Typeable)


--------------------------------------------------------------------------------
instance Exception ConnectionException


--------------------------------------------------------------------------------
data ConnectionType = ServerConnection | ClientConnection
    deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- | Replace an invalid input byte with the Unicode replacement character
-- U+FFFD.
decodeUtf8Lenient :: BL.ByteString -> TL.Text
decodeUtf8Lenient = TL.decodeUtf8With TL.lenientDecode


--------------------------------------------------------------------------------
-- | Throw an error if there is an invalid input byte.
decodeUtf8Strict :: BL.ByteString -> Either ConnectionException TL.Text
decodeUtf8Strict bl = unsafePerformIO $ try $
    let txt = TL.decodeUtf8With (\err _ -> throw (UnicodeException err)) bl in
    TL.length txt `seq` return txt
