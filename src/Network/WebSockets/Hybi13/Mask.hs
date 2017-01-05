--------------------------------------------------------------------------------
-- | Masking of fragmes using a simple XOR algorithm
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.WebSockets.Hybi13.Mask
    ( Mask
    , maskPayload
    , randomMask
    ) where


--------------------------------------------------------------------------------
import           Data.Bits            (shiftR, xor)
import           Data.Word            (Word8)
import qualified Data.ByteString.Lazy as BL
import           System.Random        (RandomGen, random)


--------------------------------------------------------------------------------
-- | ByteString should be exactly 4 bytes long
type Mask = Maybe [Word8]


--------------------------------------------------------------------------------
-- | Apply mask
maskPayload :: Mask -> BL.ByteString -> BL.ByteString
maskPayload (Just mask@[_,_,_,_]) = snd . BL.mapAccumL f 0
  where
    f !ix !c = ((ix + 1) `mod` 4, id $! (mask !! ix) `xor` c)
maskPayload _     = id

--------------------------------------------------------------------------------
-- | Create a random mask
randomMask :: forall g. RandomGen g => g -> (Mask, g)
randomMask gen = (Just [b1, b2, b3, b4], gen')
  where
    (!int, !gen') = random gen :: (Int, g)
    !b1           = fromIntegral $ int `mod` 0x100
    !b2           = fromIntegral $ int `shiftR` 8  `mod` 0x100
    !b3           = fromIntegral $ int `shiftR` 16 `mod` 0x100
    !b4           = fromIntegral $ int `shiftR` 24 `mod` 0x100
