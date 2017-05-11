{-# language BangPatterns #-}
{-# language OverloadedStrings #-}

import Criterion
import Criterion.Main

import Network.WebSockets.Hybi13.Mask

import Data.Bits (shiftR, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

setupEnv = do
    let kilo = BL.replicate 1024 37
        mega = BL.replicate (1024 * 1024) 37
        megaU = BL.fromChunks [B.drop 1 (B.replicate (1024 * 1024) 37)]
        megaS = BL.fromChunks [B.replicate (1024 * 1024) 37]
    return (kilo, mega, megaU, megaS)

maskPayload' :: Maybe B.ByteString -> BL.ByteString -> BL.ByteString
maskPayload' Nothing     = id
maskPayload' (Just mask) = snd . BL.mapAccumL f (cycle $ B.unpack mask)
  where
    f []     !c = ([], c)
    f (m:ms) !c = (ms, m `xor` c)

main = defaultMain [
    env setupEnv $ \ ~(kilo, mega, megaU, megaS) -> bgroup "main"
        [ bgroup "kilobyte payload"
            [ bgroup "zero_mask"
                [ bench "current" $ nf (maskPayload (Just 0)) kilo
                , bench "old" $ nf (maskPayload' (Just "\x00\x00\x00\x00")) kilo
                ]
            ,  bgroup "full_mask"
                [ bench "current" $ nf (maskPayload (Just 0xffffffff)) kilo
                , bench "current-unaligned" $ nf (maskPayload (Just 0xffffffff)) (BL.drop 1 kilo)
                , bench "old" $ nf (maskPayload' (Just "\xFF\xFF\xFF\xFF")) kilo
                ]
            ,  bgroup "one_byte_mask"
                [ bench "current" $ nf (maskPayload (Just 0xcccccccc)) kilo
                , bench "old" $ nf (maskPayload' (Just "\xCC\xCC\xCC\xCC")) kilo
                ]
            ,  bgroup "other_mask"
                [ bench "current" $ nf (maskPayload (Just 0xa2b0a2b0)) kilo
                , bench "old" $ nf (maskPayload' (Just "\xB0\xA2\xB0\xA2")) kilo
                ]
            ]
        , bgroup "megabyte payload"
            [ bgroup "zero_mask"
                [ bench "current" $ nf (maskPayload (Just 0)) mega
                , bench "old" $ nf (maskPayload' (Just "\x00\x00\x00\x00")) mega
                ]
            ,  bgroup "full_mask"
                [ bench "current" $ nf (maskPayload (Just 0xffffffff)) mega
                , bench "current-unaligned" $ nf (maskPayload (Just 0xffffffff)) megaU
                , bench "current-aligned" $ nf (maskPayload (Just 0xffffffff)) megaS
                , bench "old" $ nf (maskPayload' (Just "\xFF\xFF\xFF\xFF")) mega
                ]
            ,  bgroup "one_byte_mask"
                [ bench "current" $ nf (maskPayload (Just 0xcccccccc)) mega
                , bench "old" $ nf (maskPayload' (Just "\xCC\xCC\xCC\xCC")) mega
                ]
            ,  bgroup "other_mask"
                [ bench "current" $ nf (maskPayload (Just 0xa2b0a2b0)) mega
                , bench "old" $ nf (maskPayload' (Just "\xB0\xA2\xB0\xA2")) mega
                ]
            ]
        ]
    ]
