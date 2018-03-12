--------------------------------------------------------------------------------
-- | Masking of fragmes using a simple XOR algorithm
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Network.WebSockets.Hybi13.Mask
    ( Mask
    , parseMask
    , encodeMask
    , randomMask

    , maskPayload
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString.Builder       as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import           Data.Binary.Get               (Get, getWord32host)
import qualified Data.ByteString.Internal      as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Word                     (Word32, Word8)
import           Foreign.C.Types               (CChar (..), CInt (..),
                                                CSize (..))
import           Foreign.ForeignPtr            (withForeignPtr)
import           Foreign.Ptr                   (Ptr, plusPtr)
import           System.Random                 (RandomGen, random)


--------------------------------------------------------------------------------
foreign import ccall unsafe "_hs_mask_chunk" c_mask_chunk
    :: Word32 -> CInt -> Ptr CChar -> CSize -> Ptr Word8 -> IO ()


--------------------------------------------------------------------------------
-- | A mask is sequence of 4 bytes.  We store this in a 'Word32' in the host's
-- native byte ordering.
newtype Mask = Mask {unMask :: Word32}


--------------------------------------------------------------------------------
-- | Parse a mask.
parseMask :: Get Mask
parseMask = fmap Mask getWord32host


--------------------------------------------------------------------------------
-- | Encode a mask
encodeMask :: Mask -> Builder.Builder
encodeMask = Builder.word32Host . unMask


--------------------------------------------------------------------------------
-- | Create a random mask
randomMask :: forall g. RandomGen g => g -> (Mask, g)
randomMask gen = (Mask int, gen')
  where
    (!int, !gen') = random gen :: (Word32, g)


--------------------------------------------------------------------------------
-- | Mask a lazy bytestring.  Uses 'c_mask_chunk' under the hood.
maskPayload :: Maybe Mask -> BL.ByteString -> BL.ByteString
maskPayload Nothing            = id
maskPayload (Just (Mask 0))    = id
maskPayload (Just (Mask mask)) = go 0
  where
    go _           BL.Empty                               = BL.Empty
    go !maskOffset (BL.Chunk (B.PS payload off len) rest) =
        BL.Chunk maskedChunk (go ((maskOffset + len) `rem` 4) rest)
      where
        maskedChunk =
            B.unsafeCreate len $ \dst ->
            withForeignPtr payload $ \src ->
                c_mask_chunk mask
                    (fromIntegral maskOffset)
                    (src `plusPtr` off)
                    (fromIntegral len)
                    dst
