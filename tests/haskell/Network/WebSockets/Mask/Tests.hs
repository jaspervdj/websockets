{-# LANGUAGE BangPatterns      #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Mask.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.Binary.Get                      as BIN
import           Data.Bits                            (xor)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as BL
import           Network.WebSockets.Hybi13.Mask
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck                      (Arbitrary (..), (===))
import qualified Test.QuickCheck                      as QC

import           Network.WebSockets.Tests.Util
--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Masks.Tests"
    [ testProperty "correct fast masking" testMasking ]

maskPayload' :: Maybe B.ByteString -> BL.ByteString -> BL.ByteString
maskPayload' Nothing     = id
maskPayload' (Just mask) = snd . BL.mapAccumL f (cycle $ B.unpack mask)
  where
    f []     !c = ([], c)
    f (m:ms) !c = (ms, m `xor` c)

newtype AMask = AMask B.ByteString deriving (Show)
instance Arbitrary AMask where
  arbitrary = do
      c1 <- arbitrary
      c2 <- arbitrary
      c3 <- arbitrary
      c4 <- arbitrary
      return (AMask (B.pack [c1,c2,c3,c4]))

newtype APkt = APkt BL.ByteString deriving (Show)
instance Arbitrary APkt where
  arbitrary = do
    b1 <- arbitraryByteString
    b2 <- arbitraryByteString
    return $ APkt (b1 `BL.append` b2) -- Just for sure to test correctly different alignments
  shrink (APkt bs) =
      map APkt [ BL.append a b | (a, b) <- zip (BL.inits bs) (tail $ BL.tails bs) ]

testMasking :: QC.Property
testMasking =
  QC.forAllShrink QC.arbitrary QC.shrink $ \(AMask mask, APkt pkt) ->
    let wmask = BIN.runGet BIN.getWord32host (BL.fromStrict mask)
    in maskPayload' (Just mask) pkt === maskPayload (Just wmask) pkt
