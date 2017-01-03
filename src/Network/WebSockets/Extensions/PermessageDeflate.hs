{-# LANGUAGE OverloadedStrings
           , LambdaCase
           , RecordWildCards
           , BangPatterns
           #-}
module Network.WebSockets.Extensions.PermessageDeflate where

import qualified Data.ByteString                  as  BS
import qualified Data.ByteString.Char8            as BS8
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.ByteString.Lazy.Internal    as   L
import           Network.WebSockets.Types
import           Network.WebSockets.Http
import           Data.Monoid
import           Data.Either
import           Control.Applicative ((<|>), (<$>), (*>), (<*))
import           Control.Concurrent.MVar
import           Control.Monad (when)
import           Data.Streaming.Zlib
import qualified Data.Attoparsec.ByteString       as  A hiding (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as  A
import qualified Data.CaseInsensitive             as CI

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
  :: Maybe BS8.ByteString
  -> Maybe PermessageDeflate
  -> Either BS8.ByteString (Headers, Maybe PermessageDeflate)
negotiateDeflate (Just "x-webkit-deflate-frame") (Just x) = Right ([("Sec-WebSocket-Extensions", "x-webkit-deflate-frame")], Just x)
negotiateDeflate (Just prot) (Just x) | "permessage-deflate" `BS.isPrefixOf` prot =
  case partitionEithers (parseFrames prot) of
    (_, upd:_) -> let x' = upd x in Right ([(CI.mk "Sec-WebSocket-Extensions", toClient x')], Just x')
    (err:_,[]) -> Left err
    _          -> Right ([], Nothing)

negotiateDeflate _ _ = Right ([], Nothing)

{-# NOINLINE wsDecompress #-}
wsDecompress :: PermessageDeflate -> IO Inflate
wsDecompress PermessageDeflate{..} = initInflate (WindowBits (- clientMaxWindowBits))

{-# NOINLINE wsCompress #-}
wsCompress :: PermessageDeflate -> IO Deflate
wsCompress PermessageDeflate{..} = initDeflate pdCompressionLevel (WindowBits (- serverMaxWindowBits))

appTailL :: BSL.ByteString
appTailL = BSL.pack [0x00,0x00,0xff,0xff]

{-# NOINLINE wsDeflate #-}
wsDeflate :: Maybe PermessageDeflate -> IO (Message -> IO Message)
wsDeflate Nothing = return return
wsDeflate (Just pmd) = do
    dmRef <- newMVar =<< fresh
    return $ wsDeflate1 dmRef
  where
    fresh = wsCompress pmd
    compressor dmRef !x =  do
          worker <- takeMVar dmRef
          dec <- dePopper =<< feedDeflate worker (BSL.toStrict x)
          d1 <- dePopper $ flushDeflate worker
          putMVar dmRef =<< if serverNoContextTakeover pmd then fresh else return worker
          return (maybeStrip $ dec `BSL.append` d1)

    dePopper p = p >>= \case
       PRDone    -> return BSL.empty
       PRNext c  -> L.chunk c <$> dePopper p
       PRError x -> print x >> return BSL.empty

    wsDeflate1 dmRef (DataMessage (Text x))   = CompressedDataMessage . Text <$> compressor dmRef x
    wsDeflate1 dmRef (DataMessage (Binary x)) = CompressedDataMessage . Binary <$> compressor dmRef x
    wsDeflate1 _ x = return x

{-# NOINLINE wsInflate #-}
wsInflate :: Maybe PermessageDeflate -> IO (Message -> IO Message)
wsInflate Nothing = return return
wsInflate (Just pmd) = do
    dmRef <- newMVar =<< fresh
    return $ wsInflate1 dmRef
  where
    fresh = wsDecompress pmd
    compressor dmRef x =  do
          worker <- takeMVar dmRef
          dec <- dePopper =<< feedInflate worker (BSL.toStrict $ x <> appTailL)
          d1 <- flushInflate worker
          putMVar dmRef =<< if clientNoContextTakeover pmd then fresh else return worker
          return $ dec `BSL.append` BSL.fromStrict d1
    dePopper p = p >>= \case
       PRDone -> return BSL.empty
       PRNext c -> L.chunk c <$> dePopper p
       PRError x -> print x >> return BSL.empty
    wsInflate1 dmRef (CompressedDataMessage   (Text x)) = DataMessage . Text   <$> compressor dmRef x
    wsInflate1 dmRef (CompressedDataMessage (Binary x)) = DataMessage . Binary <$> compressor dmRef x
    wsInflate1 _ x = return x

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
