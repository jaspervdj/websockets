-- | Masking of fragmes using a simple XOR algorithm
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Network.WebSockets.Protocol.Hybi10.Mask
    ( Mask
    , maskPayload
    , randomMask
    ) where

import Data.Bits (shiftR, xor)
import System.Random (RandomGen, random)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- | ByteString should be exactly 4 bytes long
type Mask = Maybe B.ByteString

-- | Apply mask
maskPayload :: Mask -> BL.ByteString -> BL.ByteString
maskPayload Nothing     = id
maskPayload (Just mask) = snd . BL.mapAccumL f 0
  where
    len = B.length mask
    f !i !c = let i' = (i + 1) `mod` len
                  m = mask `B.index` i
              in (i', m `xor` c)

-- | Create a random mask
randomMask :: forall g. RandomGen g => g -> (Mask, g)
randomMask gen = (Just (B.pack [b1, b2, b3, b4]), gen')
  where
    (!int, !gen') = random gen :: (Int, g)
    !b1           = fromIntegral $ int `mod` 0x100
    !b2           = fromIntegral $ int `shiftR` 8  `mod` 0x100
    !b3           = fromIntegral $ int `shiftR` 16 `mod` 0x100
    !b4           = fromIntegral $ int `shiftR` 24 `mod` 0x100
