{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests
    ( tests
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)

import Data.Attoparsec (Result (..), parse)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), elements, oneof)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Network.WebSockets.Mask
import Network.WebSockets.Types
import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Encode as E

tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "encodeFrameDecodeFrame" encodeFrameDecodeFrame
    ]

-- | Encode a frame, then decode it again. We should obviously get our original
-- frame back
encodeFrameDecodeFrame :: ArbitraryMask -> Frame -> Bool
encodeFrameDecodeFrame (ArbitraryMask m) f =
    let lbs = Builder.toLazyByteString $ E.frame m f
        bs = B.concat $ BL.toChunks lbs
    in case parse D.frame bs of
        Done "" r -> f == r
        err       -> error ("encodeFrameDecodeFrame: " ++ show err)

newtype ArbitraryMask = ArbitraryMask Mask
                      deriving (Show)

instance Arbitrary ArbitraryMask where
    arbitrary = ArbitraryMask <$> oneof
        [ return Nothing
        , Just . B.pack <$> replicateM 4 arbitrary
        ]

instance Arbitrary FrameType where
    arbitrary = elements
        [ ContinuationFrame
        , TextFrame
        , BinaryFrame
        , CloseFrame
        , PingFrame
        , PongFrame
        ]

instance Arbitrary Frame where
    arbitrary = Frame <$> arbitrary <*> arbitrary <*> (BL.pack <$> arbitrary)
