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
import qualified Data.ByteString.Lazy as BL
import           Data.Word            (Word32, Word8)
import           Foreign.C.Types      (CChar (..), CInt (..), CSize (..))
import           Foreign.Ptr          (Ptr, plusPtr)
import           System.Random        (RandomGen, random)
import           Data.ByteString.Lazy.Internal as BL
import           Data.ByteString.Internal as BS
import           Foreign.ForeignPtr (withForeignPtr)


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
-- | This is very dangerous because it modifies the contents of the original
-- bytestring rather than returning a new one.  Use at your own risk.
maskPayload :: Mask -> BL.ByteString -> BL.ByteString
maskPayload Nothing                   = id
maskPayload (Just 0) = id
maskPayload (Just mask) = go 0
  where
    go _ Empty = Empty
    go n (Chunk (BS.PS payload off len) rest) =
        Chunk c1 (go (n + len) rest)
      where
        c1 = unsafeCreate len $ \tgt ->
              withForeignPtr payload $ \ptr ->
                  c_mask_chunk mask (fromIntegral $ n `rem` 4) (ptr `plusPtr` off) (fromIntegral len) tgt
