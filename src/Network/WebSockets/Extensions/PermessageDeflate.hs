{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Network.WebSockets.Extensions.PermessageDeflate
    ( defaultPermessageDeflate
    , PermessageDeflate(..)
    , negotiateDeflate
    , wsDeflate
    , wsInflate
    , rejectExtensions
    ) where

import           Control.Applicative                       ((*>), (<$>), (<*),
                                                            (<|>))
import           Control.Concurrent.MVar
import           Control.Exception                         (throwIO)
import           Control.Monad                             (foldM, when)
import qualified Data.Attoparsec.ByteString                as A hiding (Parser)
import qualified Data.Attoparsec.ByteString.Char8          as A
import qualified Data.ByteString                           as B
import qualified Data.ByteString                           as BS
import qualified Data.ByteString.Char8                     as BS8
import qualified Data.ByteString.Lazy                      as BSL
import qualified Data.ByteString.Lazy.Char8                as BSL8
import qualified Data.ByteString.Lazy.Internal             as L
import qualified Data.CaseInsensitive                      as CI
import           Data.Either
import           Data.Monoid
import qualified Data.Streaming.Zlib                       as Zlib
import           Network.WebSockets.Extensions
import           Network.WebSockets.Extensions.Description
import           Network.WebSockets.Http
import           Network.WebSockets.Types
import           Text.Read                                 (readMaybe)

{-
   Four extension parameters are defined for "permessage-deflate" to
   help endpoints manage per-connection resource usage.
   o  "server_no_context_takeover"
   o  "client_no_context_takeover"
   o  "server_max_window_bits"
   o  "client_max_window_bits"
-}

data PermessageDeflate = PermessageDeflate
    { serverNoContextTakeover :: Bool
    , clientNoContextTakeover :: Bool
    , serverMaxWindowBits     :: Int
    , clientMaxWindowBits     :: Int
    , pdCompressionLevel      :: Int
    } deriving Show

defaultPermessageDeflate :: PermessageDeflate
defaultPermessageDeflate = PermessageDeflate False False 15 15 8
-- defaultPermessageDeflate = PermessageDeflate True True 8 8 9

type UpdatePermessageDeflate = PermessageDeflate -> PermessageDeflate


--------------------------------------------------------------------------------
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
    param = Just . BS8.pack . show

toClient :: PermessageDeflate -> BS.ByteString
toClient = encodeExtensionDescriptions . return . toExtensionDescription


--------------------------------------------------------------------------------
negotiateDeflate :: Maybe PermessageDeflate -> NegotiateExtension
negotiateDeflate pmd0 exts = do
    (headers, pmd1) <- negotiateDeflateOpts exts pmd0
    return Extension
        { extHeaders = headers
        , extParse   = \parseRaw -> do
            inflate <- wsInflate pmd1
            return $ do
                msg <- parseRaw
                case msg of
                    Nothing -> return Nothing
                    Just m  -> fmap Just (inflate m)

        , extWrite   = \writeRaw -> do
            deflate <- wsDeflate pmd1
            return $ \msgs ->
                mapM deflate msgs >>= writeRaw
        }


--------------------------------------------------------------------------------
negotiateDeflateOpts
    :: ExtensionDescriptions
    -> Maybe PermessageDeflate
    -> Either String (Headers, Maybe PermessageDeflate)

negotiateDeflateOpts (ext : _) (Just x)
    | extName ext == "x-webkit-deflate-frame" =
        Right ([("Sec-WebSocket-Extensions", "x-webkit-deflate-frame")], Just x)

negotiateDeflateOpts (ext : exts) (Just x)
    | extName ext == "permessage-deflate" = do
        x' <- foldM setParam x (extParams ext)
        Right ([("Sec-WebSocket-Extensions", toClient x')], Just x')

negotiateDeflateOpts (_ : exts) (Just x) = negotiateDeflateOpts exts (Just x)

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
parseWindow bs8 = case readMaybe (BS8.unpack bs8) of
    Just w
        | w >= 8 && w <= 15 -> Right w
        | otherwise         -> Left $ "Window out of bounds: " ++ show w
    Nothing -> Left $ "Can't parse window: " ++ show bs8


--------------------------------------------------------------------------------
wsDecompress :: PermessageDeflate -> IO Zlib.Inflate
wsDecompress PermessageDeflate {..} =
    Zlib.initInflate (Zlib.WindowBits (- (fixWindowBits clientMaxWindowBits)))


--------------------------------------------------------------------------------
wsCompress :: PermessageDeflate -> IO Zlib.Deflate
wsCompress PermessageDeflate {..} =
    Zlib.initDeflate
        pdCompressionLevel
        (Zlib.WindowBits (- (fixWindowBits serverMaxWindowBits)))


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
appTailL :: BSL.ByteString
appTailL = BSL.pack [0x00,0x00,0xff,0xff]


--------------------------------------------------------------------------------
rejectExtensions :: Message -> IO Message
rejectExtensions (DataMessage rsv1 rsv2 rsv3 _) | rsv1 || rsv2 || rsv3 =
    throwIO $ CloseRequest 1002 "Protocol Error"
rejectExtensions x = return x


--------------------------------------------------------------------------------
{-# NOINLINE wsDeflate #-}
wsDeflate :: Maybe PermessageDeflate -> IO (Message -> IO Message)
wsDeflate Nothing = return rejectExtensions
wsDeflate (Just pmd) = do
  print pmd
  if serverNoContextTakeover pmd
      then
        return $ wsDeflate1 compressorFresh
      else
        wsDeflate1 . compressor <$> (newMVar =<< fresh)
  where
    fresh = wsCompress pmd
    compressorFresh x = flip feedDeflateLazy x =<< fresh
    compressor dmRef x =
      modifyMVar dmRef $ \worker -> (worker, ) <$> feedDeflateLazy worker x

    wsDeflate1 comp (DataMessage False False False (Text x))   = DataMessage True False False . Text <$> comp x
    wsDeflate1 comp (DataMessage False False False (Binary x)) = DataMessage True False False . Binary <$> comp x
    wsDeflate1 _ x = return x

feedDeflateLazy :: Zlib.Deflate -> BSL.ByteString -> IO BSL.ByteString
feedDeflateLazy worker x = do
  dec <- dePopper =<< Zlib.feedDeflate worker (BSL.toStrict x)
  d1  <- dePopper $ Zlib.flushDeflate worker
  return $ maybeStrip $ dec <> d1


dePopper :: IO Zlib.PopperRes -> IO BSL.ByteString
dePopper p = p >>= \case
  Zlib.PRDone    -> return BSL.empty
  Zlib.PRNext c  -> L.chunk c <$> dePopper p
  Zlib.PRError x -> throwIO $ CloseRequest 1002 (BSL8.pack (show x))


{-# NOINLINE wsInflate #-}
wsInflate :: Maybe PermessageDeflate -> IO (Message -> IO Message)
wsInflate Nothing = return rejectExtensions
wsInflate (Just pmd) =
    if clientNoContextTakeover pmd
    then
       return $ wsInflate1 compressorFresh
    else
       wsInflate1 . compressor <$> (newMVar =<< fresh)
  where
    fresh = wsDecompress pmd

    compressorFresh x = flip feedInflateLazy x =<< fresh
    compressor dmRef x =
      modifyMVar dmRef $ \worker ->
        (worker,) <$> feedInflateLazy worker x

    wsInflate1 comp (DataMessage True a b  (Text x)) = DataMessage False a b . Text   <$> comp x
    wsInflate1 comp (DataMessage True a b (Binary x)) = DataMessage False a b . Binary <$> comp x
    wsInflate1 _ x = return x

feedInflateLazy :: Zlib.Inflate -> BSL.ByteString -> IO BSL.ByteString
feedInflateLazy worker x = do
  dec <-dePopper =<< Zlib.feedInflate worker (BSL.toStrict $ x <> appTailL)
  d1 <- Zlib.flushInflate worker
  return $ dec <> BSL.fromStrict d1

maybeStrip :: BSL.ByteString -> BSL.ByteString
maybeStrip x | appTailL `BSL.isSuffixOf` x = BSL.take (BSL.length x - 4) x
maybeStrip x = x

-- tests ... negotiateDeflate (Just (pmTests !! 5)) (Just defaultPermessageDeflate)
--
-- pmTests :: [BS.ByteString]
-- pmTests=
--   [ "permessage-deflate"
--   , "permessage-deflate; client_max_window_bits; server_max_window_bits=10"
--   , "permessage-deflate; client_max_window_bits=15; server_max_window_bits=10, permessage-deflate; client_max_window_bits,permessage-deflate; client_max_window_bits=15; client_max_window_bits=10"
--   , "permessage-deflate; server_no_context_takeover, permessage-deflate; client_no_context_takeover"
--   , "permessage-deflate; client_no_context_takeover, permessage-deflate; server_no_context_takeover"
--   , "x-webkit-deflate-frame"
--   ]
