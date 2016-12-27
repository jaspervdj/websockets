{-# LANGUAGE RankNTypes
           , OverloadedStrings
           , ScopedTypeVariables
           , LambdaCase
           , TupleSections
           , RecordWildCards
           , BangPatterns
           #-}
module Network.WebSockets.Extensions.PermessageDeflate where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as L
import           Network.WebSockets.Types
import           Data.Monoid
import           Control.Concurrent.MVar
import           Data.Streaming.Zlib

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
  , pdMemoryLevel           :: Int
  }

defaultPermessageDeflate :: PermessageDeflate
defaultPermessageDeflate = PermessageDeflate False False 15 15 9 9

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
    {-# NOINLINE fresh #-}
    fresh = wsCompress pmd
    compressor dmRef !x =  do
          worker <- takeMVar dmRef
          dec <- dePopper =<< feedDeflate worker (BSL.toStrict x)
          d1 <- dePopper $ flushDeflate worker
          putMVar dmRef =<< if serverNoContextTakeover pmd then fresh else return worker
          return (maybeStrip $ dec `BSL.append` d1)

    dePopper p = p >>= \case
       PRDone    -> return BSL.empty
       PRNext c  -> fmap (L.chunk c) $ dePopper p
       PRError x -> print x >> return BSL.empty

    wsDeflate1 dmRef (DataMessage (Text x))   = fmap (CompressedDataMessage . Text  ) $ compressor dmRef x
    wsDeflate1 dmRef (DataMessage (Binary x)) = fmap (CompressedDataMessage . Binary) $ compressor dmRef x
    wsDeflate1 _ x = return x

{-# NOINLINE wsInflate #-}
wsInflate :: Maybe PermessageDeflate -> IO (Message -> IO Message)
wsInflate Nothing = return return
wsInflate (Just pmd) = do
    dmRef <- newMVar =<< fresh
    return $ wsInflate1 dmRef
  where
    {-# NOINLINE fresh #-}
    fresh = wsDecompress pmd
    compressor dmRef x =  do
          worker <- takeMVar dmRef
          dec <- dePopper =<< feedInflate worker (BSL.toStrict $ x <> appTailL)
          d1 <- flushInflate worker
          putMVar dmRef =<< if clientNoContextTakeover pmd then fresh else return worker
          return $ dec `BSL.append` BSL.fromStrict d1
    dePopper p = p >>= \case
       PRDone -> return BSL.empty
       PRNext c -> fmap (L.chunk c) $ dePopper p
       PRError x -> print x >> return BSL.empty
    wsInflate1 dmRef (CompressedDataMessage   (Text x)) = fmap (DataMessage . Text) $ compressor dmRef x
    wsInflate1 dmRef (CompressedDataMessage (Binary x)) = fmap (DataMessage . Binary) $ compressor dmRef x
    wsInflate1 _ x = return x

maybeStrip :: BSL.ByteString -> BSL.ByteString
maybeStrip x | appTailL `BSL.isSuffixOf` x = BSL.take (BSL.length x - 4) x
maybeStrip x = x
