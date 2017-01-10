{-# LANGUAGE OverloadedStrings
           , LambdaCase
           , RecordWildCards
           , TupleSections
           #-}
module Network.WebSockets.Extensions.PermessageDeflate
( defaultPermessageDeflate
, PermessageDeflate(..)
, negotiateDeflate
, wsDeflate
, wsInflate
, rejectExtensions
) where

import qualified Data.ByteString                  as  BS
import qualified Data.ByteString.Char8            as BS8
import qualified Data.ByteString.Lazy.Char8       as BSL8
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Lazy.Internal    as   L
import           Network.WebSockets.Types
import           Network.WebSockets.Http
import           Data.Monoid
import           Data.Either
import           Control.Applicative              ((<|>), (<$>), (*>), (<*))
import           Control.Concurrent.MVar
import           Control.Monad                    (when)
import           Data.Streaming.Zlib
import qualified Data.Attoparsec.ByteString       as  A hiding (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as  A
import qualified Data.CaseInsensitive             as CI
import           Control.Exception.Safe           (throwIO)

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

toClient :: PermessageDeflate -> BS.ByteString
toClient PermessageDeflate{..} = BS.intercalate "; " $
     ["permessage-deflate"]
      ++ ["server_no_context_takeover" | serverNoContextTakeover ]
      ++ ["client_no_context_takeover" | clientNoContextTakeover ]
      ++ [BS8.pack $ "server_max_window_bits=" ++ show serverMaxWindowBits | serverMaxWindowBits /= 15 ]
      ++ [BS8.pack $ "client_max_window_bits=" ++ show clientMaxWindowBits | clientMaxWindowBits /= 15 ]

negotiateDeflate
  :: [BS8.ByteString]
  -> Maybe PermessageDeflate
  -> Either String (Headers, Maybe PermessageDeflate)
negotiateDeflate ("x-webkit-deflate-frame":_) (Just x) = Right ([("Sec-WebSocket-Extensions", "x-webkit-deflate-frame")], Just x)
negotiateDeflate (prot:exts) (Just x) | "permessage-deflate" `BS.isPrefixOf` prot =
  case partitionEithers (parseFrames prot) of
    (_, upd:_) -> let x' = upd x in Right ([(CI.mk "Sec-WebSocket-Extensions", toClient x')], Just x')
    (err:_,[]) -> Left (BS8.unpack err)
    _          -> negotiateDeflate exts (Just x)
negotiateDeflate (_:exts) (Just x) = negotiateDeflate exts (Just x)
negotiateDeflate _ _ = Right ([], Nothing)

{-# NOINLINE wsDecompress #-}
wsDecompress :: PermessageDeflate -> IO Inflate
wsDecompress PermessageDeflate{..} = initInflate (WindowBits (- clientMaxWindowBits))

{-# NOINLINE wsCompress #-}
wsCompress :: PermessageDeflate -> IO Deflate
wsCompress PermessageDeflate{..} = initDeflate pdCompressionLevel (WindowBits (- serverMaxWindowBits))

appTailL :: BSL.ByteString
appTailL = BSL.pack [0x00,0x00,0xff,0xff]

rejectExtensions :: Message -> IO Message
rejectExtensions (DataMessage a b c _) | a || b || c = throwIO $ CloseRequest 1002 "Protocol Error"
rejectExtensions a = return a

{-# NOINLINE wsDeflate #-}
wsDeflate :: Maybe PermessageDeflate -> IO (Message -> IO Message)
wsDeflate Nothing = return rejectExtensions
wsDeflate (Just pmd) =
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

feedDeflateLazy :: Deflate -> BSL.ByteString -> IO BSL.ByteString
feedDeflateLazy worker x = do
  dec <- dePopper =<< feedDeflate worker (BSL.toStrict x)
  d1 <- dePopper $ flushDeflate worker
  return $ maybeStrip $ dec <> d1


dePopper :: IO PopperRes -> IO BSL.ByteString
dePopper p = p >>= \case
  PRDone    -> return BSL.empty
  PRNext c  -> L.chunk c <$> dePopper p
  PRError x -> throwIO $ CloseRequest 1002 (BSL8.pack (show x))


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

feedInflateLazy :: Inflate -> BSL.ByteString -> IO BSL.ByteString
feedInflateLazy worker x = do
  dec <-dePopper =<< feedInflate worker (BSL.toStrict $ x <> appTailL)
  d1 <- flushInflate worker
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

noServerContext :: A.Parser UpdatePermessageDeflate
noServerContext = A.skipSpace *> A.string "server_no_context_takeover" *> return (\x -> x{serverNoContextTakeover=True}) <* A.skipSpace

noClientContext :: A.Parser UpdatePermessageDeflate
noClientContext = A.skipSpace *> A.string "client_no_context_takeover" *> return (\x -> x{clientNoContextTakeover=True}) <* A.skipSpace

serverMaxWindowBitsP :: A.Parser UpdatePermessageDeflate
serverMaxWindowBitsP = do
   _ <- A.skipSpace *> A.string "server_max_window_bits"
   w <- window <* A.skipSpace
   return (\x -> x{serverMaxWindowBits=w})

clientMaxWindowBitsP :: A.Parser UpdatePermessageDeflate
clientMaxWindowBitsP = do
   _ <- A.skipSpace *> A.string "client_max_window_bits"
   w <- window <|> return 15
   A.skipSpace
   return (\x -> x{clientMaxWindowBits=w})

window :: A.Parser Int
window = do
   A.skipSpace >> A.char '=' >> A.skipSpace
   ret <- A.decimal <|> A.char '"' *> A.decimal <* A.char '"'
   when (ret < 8 || ret > 15) $ fail "outofBounds"
   return ret

parseFrame :: A.Parser UpdatePermessageDeflate
parseFrame = do
   A.string "permessage-deflate" *> A.skipSpace
   t <- (A.char ';' *> A.sepBy1' (A.choice [noServerContext, noClientContext, serverMaxWindowBitsP, clientMaxWindowBitsP] <* A.skipSpace) (A.char ';')
     ) <|> return []
   A.skipSpace
   return $ foldr (flip (.)) id t

parseFrames:: BS8.ByteString
     -> [Either BS8.ByteString UpdatePermessageDeflate]
parseFrames = map (pa (A.skipSpace *> parseFrame <* A.skipSpace)) . extensions

pa :: A.Parser b -> BS8.ByteString -> Either BS8.ByteString b
pa p = either (Left . BS8.pack) Right . A.parseOnly p

extensions :: BS8.ByteString -> [BS8.ByteString]
extensions = BS8.split ','
