-- | Masking of fragmes using a simple XOR algorithm
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Network.WebSockets.Protocol.Hybi10.Mask
    ( Mask
    , maskPayload
    ) where

import Data.Bits (xor)

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
