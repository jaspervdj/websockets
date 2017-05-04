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
import           Data.Bits            (shiftR)
import           Data.Bits            (shiftL, (.|.))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word            (Word32)
import           Foreign.C.Types      (CChar (..), CInt (..), CSize (..))
import           Foreign.Ptr          (Ptr)
import           System.IO.Unsafe     (unsafePerformIO)
import           System.Random        (RandomGen, random)


--------------------------------------------------------------------------------
foreign import ccall unsafe "_hs_mask_chunk" c_mask_chunk
    :: Word32 -> CInt -> Ptr CChar -> CSize -> IO CInt


--------------------------------------------------------------------------------
-- | ByteString should be exactly 4 bytes long
type Mask = Maybe B.ByteString


--------------------------------------------------------------------------------
-- | Create a random mask
randomMask :: forall g. RandomGen g => g -> (Mask, g)
randomMask gen = (Just (B.pack [b1, b2, b3, b4]), gen')
  where
    (!int, !gen') = random gen :: (Int, g)
    !b1           = fromIntegral $ int `mod` 0x100
    !b2           = fromIntegral $ int `shiftR` 8  `mod` 0x100
    !b3           = fromIntegral $ int `shiftR` 16 `mod` 0x100
    !b4           = fromIntegral $ int `shiftR` 24 `mod` 0x100


--------------------------------------------------------------------------------
-- | This is very dangerous because it modifies the contents of the original
-- bytestring rather than returning a new one.  Use at your own risk.
maskPayload :: Mask -> BL.ByteString -> BL.ByteString
maskPayload Nothing                   = id
maskPayload (Just "\x00\x00\x00\x00") = id
maskPayload (Just mask)
    | B.length mask == 4 =
        BL.fromChunks . unsafePerformIO . go 0 . BL.toChunks
    | otherwise          =
        error "Network.WebSockets.Hybi13.Mask: mask length must be 4"
  where
    go :: CInt -> [B.ByteString] -> IO [B.ByteString]
    go _ [] = return []
    go shift0 (c0 : chunks) = do
        -- TODO (jaspervdj): this code copies the string twice:
        --
        -- - Once in 'B.useAsCStringLen'
        -- - Once in 'B.packCStringLen'
        --
        -- We should be able to get away with one copy and in some cases zero
        -- copies.
        (c1, shift1) <- B.useAsCStringLen c0 $ \(ptr, len) -> do
            shift1 <- c_mask_chunk mask32 shift0 ptr (fromIntegral len)
            c1     <- B.packCStringLen (ptr, len)
            return (c1, shift1)
        (c1 :) <$> go shift1 chunks

    -- | Puts the mask into a 'Word32' in a way that will allow fast masking on
    -- little-endian platforms.
    mask32 :: Word32
    mask32 =
        (fromIntegral (B.index mask 0) `shiftL`  0) .|.
        (fromIntegral (B.index mask 1) `shiftL`  8) .|.
        (fromIntegral (B.index mask 2) `shiftL` 16) .|.
        (fromIntegral (B.index mask 3) `shiftL` 24)
