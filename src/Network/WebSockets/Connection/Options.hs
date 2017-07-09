--------------------------------------------------------------------------------
module Network.WebSockets.Connection.Options
    ( ConnectionOptions (..)
    , defaultConnectionOptions

    , CompressionOptions (..)
    , PermessageDeflate (..)
    , defaultPermessageDeflate

    , FrameSizeLimit (..)
    , MessageDataSizeLimit (..)
    ) where


--------------------------------------------------------------------------------
import           Data.Int (Int64)


--------------------------------------------------------------------------------
-- | Set options for a 'Connection'.
data ConnectionOptions = ConnectionOptions
    { connectionOnPong               :: !(IO ())
      -- ^ Whenever a 'pong' is received, this IO action is executed. It can be
      -- used to tickle connections or fire missiles.
    , connectionCompressionOptions   :: !CompressionOptions
      -- ^ Enable 'PermessageDeflate'.
    , connectionStrictUnicode        :: !Bool
      -- ^ Enable strict unicode on the connection.  This means that if a client
      -- (or server) sends invalid UTF-8, we will throw a 'UnicodeException'
      -- rather than replacing it by the unicode replacement character U+FFFD.
    , connectionFrameSizeLimit       :: !FrameSizeLimit
      -- ^ The maximum size for incoming frames, in bytes.  If a frame exceeds
      -- this limit, a 'ParseException' is thrown.
    , connectionMessageDataSizeLimit :: !MessageDataSizeLimit
      -- ^ 'connectionFrameSizeLimit' is often not enough since a malicious
      -- client can send many small frames to create a huge message.  This limit
      -- allows you to protect from that.  If a message exceeds this limit, a
      -- 'ParseException' is thrown.
      --
      -- Note that, if compression is enabled, we check the size of the
      -- compressed messages, as well as the size of the uncompressed messages
      -- as we are deflating them to ensure we don't use too much memory in any
      -- case.
    }


--------------------------------------------------------------------------------
-- | The default connection options:
--
-- * Nothing happens when a pong is received.
-- * Compression is disabled.
-- * Lenient unicode decoding.
defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { connectionOnPong               = return ()
    , connectionCompressionOptions   = NoCompression
    , connectionStrictUnicode        = False
    , connectionFrameSizeLimit       = NoFrameSizeLimit
    , connectionMessageDataSizeLimit = NoMessageDataSizeLimit
    }


--------------------------------------------------------------------------------
data CompressionOptions
    = NoCompression
    | PermessageDeflateCompression PermessageDeflate
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Four extension parameters are defined for "permessage-deflate" to
-- help endpoints manage per-connection resource usage.
--
-- - "server_no_context_takeover"
-- - "client_no_context_takeover"
-- - "server_max_window_bits"
-- - "client_max_window_bits"
data PermessageDeflate = PermessageDeflate
    { serverNoContextTakeover :: Bool
    , clientNoContextTakeover :: Bool
    , serverMaxWindowBits     :: Int
    , clientMaxWindowBits     :: Int
    , pdCompressionLevel      :: Int
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
defaultPermessageDeflate :: PermessageDeflate
defaultPermessageDeflate = PermessageDeflate False False 15 15 8


--------------------------------------------------------------------------------
data FrameSizeLimit
    = NoFrameSizeLimit
    | FrameSizeLimit !Int64
    deriving (Eq, Show)


--------------------------------------------------------------------------------
data MessageDataSizeLimit
    = NoMessageDataSizeLimit
    | MessageDataSizeLimit !Int64
    deriving (Eq, Show)
