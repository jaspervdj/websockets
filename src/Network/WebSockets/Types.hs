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
    ) where


--------------------------------------------------------------------------------
import           Control.Exception       (Exception (..))
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Typeable           (Typeable)


--------------------------------------------------------------------------------
import           Network.WebSockets.Http


--------------------------------------------------------------------------------
-- | The kind of message a server application typically deals with
data Message
    = ControlMessage ControlMessage
    | DataMessage DataMessage
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Different control messages
data ControlMessage
    = Close BL.ByteString
    | Ping BL.ByteString
    | Pong BL.ByteString
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | For an end-user of this library, dealing with 'Frame's would be a bit
-- low-level. This is why define another type on top of it, which represents
-- data for the application layer.
data DataMessage
    = Text BL.ByteString
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
    fromLazyByteString :: BL.ByteString -> a
    toLazyByteString   :: a -> BL.ByteString


--------------------------------------------------------------------------------
instance WebSocketsData BL.ByteString where
    fromLazyByteString = id
    toLazyByteString   = id


--------------------------------------------------------------------------------
instance WebSocketsData B.ByteString where
    fromLazyByteString = B.concat . BL.toChunks
    toLazyByteString   = BL.fromChunks . return


--------------------------------------------------------------------------------
instance WebSocketsData TL.Text where
    fromLazyByteString = TL.decodeUtf8
    toLazyByteString   = TL.encodeUtf8


--------------------------------------------------------------------------------
instance WebSocketsData T.Text where
    fromLazyByteString = T.concat . TL.toChunks . fromLazyByteString
    toLazyByteString   = toLazyByteString . TL.fromChunks . return


--------------------------------------------------------------------------------
-- | The connection couldn't be established or broke down unexpectedly. thrown
-- as an iteratee exception.
data ConnectionException
    -- | the client unexpectedly closed the connection while we were trying to
    -- receive some data.
    --
    -- todo: Also want this for sending.
    = ConnectionClosed
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Exception ConnectionException
