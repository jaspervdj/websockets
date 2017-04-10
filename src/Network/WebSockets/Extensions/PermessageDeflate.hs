--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Network.WebSockets.Extensions.PermessageDeflate
    ( defaultPermessageDeflate
    , PermessageDeflate(..)
    , negotiateDeflate
    ) where


--------------------------------------------------------------------------------
import           Control.Exception                         (throwIO)
import           Control.Monad                             (foldM)
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Char8                     as B8
import qualified Data.ByteString.Lazy                      as BL
import qualified Data.ByteString.Lazy.Char8                as BL8
import qualified Data.ByteString.Lazy.Internal             as BL
import           Data.Monoid
import qualified Data.Streaming.Zlib                       as Zlib
import           Network.WebSockets.Extensions
import           Network.WebSockets.Extensions.Description
import           Network.WebSockets.Http
import           Network.WebSockets.Types
import           Text.Read                                 (readMaybe)

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
    } deriving Show


--------------------------------------------------------------------------------
defaultPermessageDeflate :: PermessageDeflate
defaultPermessageDeflate = PermessageDeflate False False 15 15 8


--------------------------------------------------------------------------------
-- | Convert the parameters to an 'ExtensionDescription' that we can put in a
-- 'Sec-WebSocket-Extensions' header.
toExtensionDescription :: PermessageDeflate -> ExtensionDescription
toExtensionDescription PermessageDeflate {..} = ExtensionDescription
    { extName   = "permessage-deflate"
    , extParams =
         [("server_no_context_takeover", Nothing) | serverNoContextTakeover] ++
         [("client_no_context_takeover", Nothing) | clientNoContextTakeover] ++
         [("server_max_window_bits", param serverMaxWindowBits) | serverMaxWindowBits /= 15] ++
         [("client_max_window_bits", param clientMaxWindowBits) | clientMaxWindowBits /= 15]
    }
  where
    param = Just . B8.pack . show


--------------------------------------------------------------------------------
toHeaders :: PermessageDeflate -> Headers
toHeaders pmd =
    [ ( "Sec-WebSocket-Extensions"
      , encodeExtensionDescriptions [toExtensionDescription pmd]
      )
    ]


--------------------------------------------------------------------------------
negotiateDeflate :: Maybe PermessageDeflate -> NegotiateExtension
negotiateDeflate pmd0 exts0 = do
    (headers, pmd1) <- negotiateDeflateOpts exts0 pmd0
    return Extension
        { extHeaders = headers
        , extParse   = \parseRaw -> do
            inflate <- makeMessageInflater pmd1
            return $ do
                msg <- parseRaw
                case msg of
                    Nothing -> return Nothing
                    Just m  -> fmap Just (inflate m)

        , extWrite   = \writeRaw -> do
            deflate <- makeMessageDeflater pmd1
            return $ \msgs ->
                mapM deflate msgs >>= writeRaw
        }
  where
    negotiateDeflateOpts
        :: ExtensionDescriptions
        -> Maybe PermessageDeflate
        -> Either String (Headers, Maybe PermessageDeflate)

    negotiateDeflateOpts (ext : _) (Just x)
        | extName ext == "x-webkit-deflate-frame" = Right
            ([("Sec-WebSocket-Extensions", "x-webkit-deflate-frame")], Just x)

    negotiateDeflateOpts (ext : _) (Just x)
        | extName ext == "permessage-deflate" = do
            x' <- foldM setParam x (extParams ext)
            Right (toHeaders x', Just x')

    negotiateDeflateOpts (_ : exts) (Just x) =
        negotiateDeflateOpts exts (Just x)

    negotiateDeflateOpts _ _ = Right ([], Nothing)


--------------------------------------------------------------------------------
setParam
    :: PermessageDeflate -> ExtensionParam -> Either String PermessageDeflate
setParam pmd ("server_no_context_takeover", _) =
    Right pmd {serverNoContextTakeover = True}

setParam pmd ("client_no_context_takeover", _) =
    Right pmd {clientNoContextTakeover = True}

setParam pmd ("server_max_window_bits", Nothing) =
    Right pmd {serverMaxWindowBits = 15}

setParam pmd ("server_max_window_bits", Just param) = do
    w <- parseWindow param
    Right pmd {serverMaxWindowBits = w}

setParam pmd ("client_max_window_bits", Nothing) = do
    Right pmd {clientMaxWindowBits = 15}

setParam pmd ("client_max_window_bits", Just param) = do
    w <- parseWindow param
    Right pmd {clientMaxWindowBits = w}

setParam pmd (_, _) = Right pmd


--------------------------------------------------------------------------------
parseWindow :: B.ByteString -> Either String Int
parseWindow bs8 = case readMaybe (B8.unpack bs8) of
    Just w
        | w >= 8 && w <= 15 -> Right w
        | otherwise         -> Left $ "Window out of bounds: " ++ show w
    Nothing -> Left $ "Can't parse window: " ++ show bs8


--------------------------------------------------------------------------------
-- | If the window_bits parameter is set to 8, we must set it to 9 instead.
--
-- Related issues:
-- - https://github.com/haskell/zlib/issues/11
-- - https://github.com/madler/zlib/issues/94
--
-- Quote from zlib manual:
--
-- For the current implementation of deflate(), a windowBits value of 8 (a
-- window size of 256 bytes) is not supported. As a result, a request for 8 will
-- result in 9 (a 512-byte window). In that case, providing 8 to inflateInit2()
-- will result in an error when the zlib header with 9 is checked against the
-- initialization of inflate(). The remedy is to not use 8 with deflateInit2()
-- with this initialization, or at least in that case use 9 with inflateInit2().
fixWindowBits :: Int -> Int
fixWindowBits n
    | n < 9     = 9
    | n > 15    = 15
    | otherwise = n


--------------------------------------------------------------------------------
appTailL :: BL.ByteString
appTailL = BL.pack [0x00,0x00,0xff,0xff]


--------------------------------------------------------------------------------
maybeStrip :: BL.ByteString -> BL.ByteString
maybeStrip x | appTailL `BL.isSuffixOf` x = BL.take (BL.length x - 4) x
maybeStrip x = x


--------------------------------------------------------------------------------
rejectExtensions :: Message -> IO Message
rejectExtensions (DataMessage rsv1 rsv2 rsv3 _) | rsv1 || rsv2 || rsv3 =
    throwIO $ CloseRequest 1002 "Protocol Error"
rejectExtensions x = return x


--------------------------------------------------------------------------------
makeMessageDeflater
    :: Maybe PermessageDeflate -> IO (Message -> IO Message)
makeMessageDeflater Nothing = return rejectExtensions
makeMessageDeflater (Just pmd)
    | serverNoContextTakeover pmd = do
        return $ \msg -> do
            ptr <- initDeflate pmd
            deflateMessageWith (deflateBody ptr) msg
    | otherwise = do
        ptr <- initDeflate pmd
        return $ \msg ->
            deflateMessageWith (deflateBody ptr) msg
  where
    ----------------------------------------------------------------------------
    initDeflate :: PermessageDeflate -> IO Zlib.Deflate
    initDeflate PermessageDeflate {..} =
        Zlib.initDeflate
            pdCompressionLevel
            (Zlib.WindowBits (- (fixWindowBits serverMaxWindowBits)))


    ----------------------------------------------------------------------------
    deflateMessageWith
        :: (BL.ByteString -> IO BL.ByteString)
        -> Message -> IO Message
    deflateMessageWith deflater (DataMessage False False False (Text x))   =
        DataMessage True False False . Text <$> deflater x
    deflateMessageWith deflater (DataMessage False False False (Binary x)) =
        DataMessage True False False . Binary <$> deflater x
    deflateMessageWith _ x = return x


    ----------------------------------------------------------------------------
    deflateBody :: Zlib.Deflate -> BL.ByteString -> IO BL.ByteString
    deflateBody ptr = fmap maybeStrip . go . BL.toChunks
      where
        go []       = dePopper (Zlib.flushDeflate ptr)
        go (c : cs) = do
            bl <- Zlib.feedDeflate ptr c >>= dePopper
            (bl <>) <$> go cs


--------------------------------------------------------------------------------
dePopper :: Zlib.Popper -> IO BL.ByteString
dePopper p = p >>= \case
    Zlib.PRDone    -> return BL.empty
    Zlib.PRNext c  -> BL.chunk c <$> dePopper p
    Zlib.PRError x -> throwIO $ CloseRequest 1002 (BL8.pack (show x))


--------------------------------------------------------------------------------
makeMessageInflater :: Maybe PermessageDeflate -> IO (Message -> IO Message)
makeMessageInflater Nothing = return rejectExtensions
makeMessageInflater (Just pmd)
    | clientNoContextTakeover pmd =
        return $ \msg -> do
            ptr <- initInflate pmd
            inflateMessageWith (inflateBody ptr) msg
    | otherwise = do
        ptr <- initInflate pmd
        return $ \msg ->
            inflateMessageWith (inflateBody ptr) msg
  where
    --------------------------------------------------------------------------------
    initInflate :: PermessageDeflate -> IO Zlib.Inflate
    initInflate PermessageDeflate {..} =
        Zlib.initInflate
            (Zlib.WindowBits (- (fixWindowBits clientMaxWindowBits)))


    ----------------------------------------------------------------------------
    inflateMessageWith
        :: (BL.ByteString -> IO BL.ByteString)
        -> Message -> IO Message
    inflateMessageWith inflater (DataMessage True a b (Text x)) =
        DataMessage False a b . Text <$> inflater x
    inflateMessageWith inflater (DataMessage True a b (Binary x)) =
        DataMessage False a b . Binary <$> inflater x
    inflateMessageWith _ x = return x


    ----------------------------------------------------------------------------
    inflateBody :: Zlib.Inflate -> BL.ByteString -> IO BL.ByteString
    inflateBody ptr =
        go . BL.toChunks . (<> appTailL)
      where
        go []       = BL.fromStrict <$> Zlib.flushInflate ptr
        go (c : cs) = do
            bl <- Zlib.feedInflate ptr c >>= dePopper
            (bl <>) <$> go cs
