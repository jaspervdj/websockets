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
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           System.Random        (RandomGen, random)


--------------------------------------------------------------------------------
-- | ByteString should be exactly 4 bytes long
type Mask = Maybe B.ByteString


--------------------------------------------------------------------------------
-- | Apply mask
maskPayload :: Mask -> BL.ByteString -> BL.ByteString
maskPayload Nothing     = id
maskPayload (Just mask) = snd . BL.mapAccumL f (cycle $ B.unpack mask)
  where
    f []     !c = ([], c)
    f (m:ms) !c = (ms, m `xor` c)

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
