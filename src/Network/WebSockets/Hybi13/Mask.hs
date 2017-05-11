--------------------------------------------------------------------------------
-- | Masking of fragmes using a simple XOR algorithm
{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Network.WebSockets.Hybi13.Mask
    ( Mask
    , maskPayload
    , randomMask
    ) where


--------------------------------------------------------------------------------
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
-- | ByteString should be exactly 4 bytes long
type Mask = Maybe Word32


--------------------------------------------------------------------------------
-- | Create a random mask
randomMask :: forall g. RandomGen g => g -> (Mask, g)
randomMask gen = (Just int, gen')
  where
    (!int, !gen') = random gen :: (Word32, g)


--------------------------------------------------------------------------------
-- | Mask a lazy bytestring.  Uses 'c_mask_chunk' under the hood.
maskPayload :: Mask -> BL.ByteString -> BL.ByteString
maskPayload Nothing     = id
maskPayload (Just 0)    = id
maskPayload (Just mask) = go 0
  where
    go _          BL.Empty                               = BL.Empty
    go !maskShift (BL.Chunk (B.PS payload off len) rest) =
        BL.Chunk maskedChunk (go ((maskShift + len) `rem` 4) rest)
      where
        maskedChunk =
            B.unsafeCreate len $ \dst ->
            withForeignPtr payload $ \src ->
                c_mask_chunk mask
                    (fromIntegral maskShift)
                    (src `plusPtr` off)
                    (fromIntegral len)
                    dst
