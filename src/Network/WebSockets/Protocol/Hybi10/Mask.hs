-- | Masking of fragmes using a simple XOR algorithm
{-# LANGUAGE BangPatterns #-}
module Network.WebSockets.Protocol.Hybi10.Mask
    ( Mask
    , maskPayload
    , randomMask
    ) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.Bits (xor)
import System.Random (randomRIO)

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
randomMask :: IO Mask
randomMask = Just . B.pack <$> replicateM 4 randomByte
  where
    randomByte = fromIntegral <$> randomRIO (0x00 :: Int, 0xff)
