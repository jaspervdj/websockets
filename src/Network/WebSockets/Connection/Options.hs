{-# LANGUAGE CPP #-}
--------------------------------------------------------------------------------
module Network.WebSockets.Connection.Options
    ( ConnectionOptions (..)
    , defaultConnectionOptions

    , CompressionOptions (..)
    , PermessageDeflate (..)
    , defaultPermessageDeflate

    , SizeLimit (..)
    , atMostSizeLimit
    ) where


--------------------------------------------------------------------------------
import           Data.Int    (Int64)
import           Data.Monoid (Monoid (..))
import           Prelude


--------------------------------------------------------------------------------
-- | Set options for a 'Connection'.  Please do not use this constructor
-- directly, but rather use 'defaultConnectionOptions' and then set the fields
-- you want, e.g.:
--
-- > myOptions = defaultConnectionOptions {connectionStrictUnicode = True}
--
-- This way your code does not break if the library introduces new fields.
data ConnectionOptions = ConnectionOptions
    { connectionOnPong                :: !(IO ())
      -- ^ Whenever a 'pong' is received, this IO action is executed. It can be
      -- used to tickle connections or fire missiles.
    , connectionTimeout               :: !Int
      -- ^ Timeout for connection establishment in seconds. Only used in the client.
    , connectionCompressionOptions    :: !CompressionOptions
      -- ^ Enable 'PermessageDeflate'.
    , connectionStrictUnicode         :: !Bool
      -- ^ Enable strict unicode on the connection.  This means that if a client
      -- (or server) sends invalid UTF-8, we will throw a 'UnicodeException'
      -- rather than replacing it by the unicode replacement character U+FFFD.
    , connectionFramePayloadSizeLimit :: !SizeLimit
      -- ^ The maximum size for incoming frame payload size in bytes.  If a
      -- frame exceeds this limit, a 'ParseException' is thrown.
    , connectionMessageDataSizeLimit  :: !SizeLimit
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
-- * 30 second timeout for connection establishment.
defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { connectionOnPong                = return ()
    , connectionTimeout               = 30
    , connectionCompressionOptions    = NoCompression
    , connectionStrictUnicode         = False
    , connectionFramePayloadSizeLimit = mempty
    , connectionMessageDataSizeLimit  = mempty
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
-- | A size limit, in bytes.  The 'Monoid' instance takes the minimum limit.
data SizeLimit
    = NoSizeLimit
    | SizeLimit !Int64
    deriving (Eq, Show)


--------------------------------------------------------------------------------
instance Monoid SizeLimit where
    mempty = NoSizeLimit

#if !MIN_VERSION_base(4,11,0)
    mappend NoSizeLimit   y             = y
    mappend x             NoSizeLimit   = x
    mappend (SizeLimit x) (SizeLimit y) = SizeLimit (min x y)
#else
instance Semigroup SizeLimit where
    (<>)    NoSizeLimit   y             = y
    (<>)    x             NoSizeLimit   = x
    (<>)    (SizeLimit x) (SizeLimit y) = SizeLimit (min x y)
#endif

--------------------------------------------------------------------------------
atMostSizeLimit :: Int64 -> SizeLimit -> Bool
atMostSizeLimit _ NoSizeLimit   = True
atMostSizeLimit s (SizeLimit l) = s <= l
{-# INLINE atMostSizeLimit #-}
