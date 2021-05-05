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

    , TLSSettings (..)
    , defaultTlsSettings

    , CertSettings (..)
    , defaultCertSettings
    ) where


--------------------------------------------------------------------------------
import           Data.Int    (Int64)
import           Data.Monoid (Monoid (..))
import           Prelude

import qualified Crypto.PubKey.DH           as DH
import qualified Data.ByteString            as B
import           Data.Default.Class           (def)
import qualified Data.IORef                 as IO
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLSExtra
import qualified Network.TLS.SessionManager as SM


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
    , connectionTlsSettings           :: !(Maybe TLSSettings)
    }


--------------------------------------------------------------------------------
-- | The default connection options:
--
-- * Nothing happens when a pong is received.
-- * Compression is disabled.
-- * Lenient unicode decoding.
defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
    { connectionOnPong                = return ()
    , connectionCompressionOptions    = NoCompression
    , connectionStrictUnicode         = False
    , connectionFramePayloadSizeLimit = mempty
    , connectionMessageDataSizeLimit  = mempty
    , connectionTlsSettings           = Nothing
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

--------------------------------------------------------------------------------
-- | Determines where to load the certificate, chain
-- certificates, and key from.
data CertSettings
  = CertFromFile !FilePath ![FilePath] !FilePath
  | CertFromMemory !B.ByteString ![B.ByteString] !B.ByteString
  | CertFromRef !(IO.IORef B.ByteString) ![IO.IORef B.ByteString] !(IO.IORef B.ByteString)

-- | The default 'CertSettings'.
defaultCertSettings :: CertSettings
defaultCertSettings = CertFromFile "certificate.pem" [] "key.pem"

--------------------------------------------------------------------------------
data TLSSettings = TLSSettings {
    certSettings :: CertSettings
    -- ^ Where are the certificate, chain certificates, and key
    -- loaded from?
    --
    -- >>> certSettings defaultTlsSettings
    -- tlsSettings "certificate.pem" "key.pem"
  , tlsLogging :: TLS.Logging
    -- ^ The level of logging to turn on.
    --
    -- Default: 'TLS.defaultLogging'.
  , tlsAllowedVersions :: [TLS.Version]
    -- ^ The TLS versions this server accepts.
    --
    -- >>> tlsAllowedVersions defaultTlsSettings
    -- [TLS13,TLS12,TLS11,TLS10]
  , tlsCiphers :: [TLS.Cipher]
    -- ^ The TLS ciphers this server accepts.
    --
    -- >>> tlsCiphers defaultTlsSettings
    -- [ECDHE-ECDSA-AES256GCM-SHA384,ECDHE-ECDSA-AES128GCM-SHA256,ECDHE-RSA-AES256GCM-SHA384,ECDHE-RSA-AES128GCM-SHA256,DHE-RSA-AES256GCM-SHA384,DHE-RSA-AES128GCM-SHA256,ECDHE-ECDSA-AES256CBC-SHA384,ECDHE-RSA-AES256CBC-SHA384,DHE-RSA-AES256-SHA256,ECDHE-ECDSA-AES256CBC-SHA,ECDHE-RSA-AES256CBC-SHA,DHE-RSA-AES256-SHA1,RSA-AES256GCM-SHA384,RSA-AES256-SHA256,RSA-AES256-SHA1,AES128GCM-SHA256,AES256GCM-SHA384]
  , tlsWantClientCert :: Bool
    -- ^ Whether or not to demand a certificate from the client.  If this
    -- is set to True, you must handle received certificates in a server hook
    -- or all connections will fail.
    --
    -- >>> tlsWantClientCert defaultTlsSettings
    -- False
  , tlsServerHooks :: TLS.ServerHooks
    -- ^ The server-side hooks called by the tls package, including actions
    -- to take when a client certificate is received.  See the "Network.TLS"
    -- module for details.
    --
    -- Default: def
  , tlsServerDHEParams :: Maybe DH.Params
    -- ^ Configuration for ServerDHEParams
    -- more function lives in `cryptonite` package
    --
    -- Default: Nothing
  , tlsSessionManagerConfig :: Maybe SM.Config
    -- ^ Configuration for in-memory TLS session manager.
    -- If Nothing, 'TLS.noSessionManager' is used.
    -- Otherwise, an in-memory TLS session manager is created
    -- according to 'Config'.
    --
    -- Default: Nothing
  , tlsCredentials :: Maybe TLS.Credentials
    -- ^ Specifying 'TLS.Credentials' directly.  If this value is
    --   specified, other fields such as 'certFile' are ignored.
  , tlsSessionManager :: Maybe TLS.SessionManager
    -- ^ Specifying 'TLS.SessionManager' directly. If this value is
    --   specified, 'tlsSessionManagerConfig' is ignored.
  }

defaultTlsSettings :: TLSSettings
defaultTlsSettings =
  TLSSettings
  { certSettings = defaultCertSettings
  , tlsLogging = def
  , tlsAllowedVersions = [TLS.TLS13,TLS.TLS12]
  , tlsCiphers = ciphers
  , tlsWantClientCert = False
  , tlsServerHooks = def
  , tlsServerDHEParams = Nothing
  , tlsSessionManagerConfig = Nothing
  , tlsCredentials = Nothing
  , tlsSessionManager = Nothing
  }
 where
   -- taken from stunnel example in tls-extra
   ciphers :: [TLS.Cipher]
   ciphers = TLSExtra.ciphersuite_strong
