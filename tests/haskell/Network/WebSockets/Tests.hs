{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)

import Data.Attoparsec (Result (..), parse)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Gen, elements, oneof)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Network.WebSockets.Mask
import Network.WebSockets.Types
import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Encode as E

import Network.WebSockets.Protocol.Hybi10 (hybi10)
import Network.WebSockets.Protocol.Hybi00 (hybi00)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "encodeFrameDecodeFrame-hybi10" (encodeFrameDecodeFrame hybi10)
    , testProperty "encodeFrameDecodeFrame-hybi00" (encodeFrameDecodeFrameFor00 hybi00)
    ]

-- | Encode a frame, then decode it again. We should obviously get our original
-- frame back
encodeFrameDecodeFrame :: Protocol -> ArbitraryMask -> Frame -> Bool
encodeFrameDecodeFrame proto (ArbitraryMask m) f =
    let lbs = Builder.toLazyByteString $ (encodeFrame proto) m f
        bs = B.concat $ BL.toChunks lbs
    in case parse (decodeFrame proto) bs of
        Done "" r -> f == r
        err       -> error ("encodeFrameDecodeFrame: " ++ show err)

encodeFrameDecodeFrameFor00 :: Protocol -> ArbitraryMask -> ArbitraryFrameFor00 -> Bool
encodeFrameDecodeFrameFor00 proto am (ArbitraryFrameFor00 f) = encodeFrameDecodeFrame proto am f

newtype ArbitraryFrameFor00 = ArbitraryFrameFor00 Frame
    deriving (Show)

instance Arbitrary ArbitraryFrameFor00 where
    arbitrary = ArbitraryFrameFor00 <$>
        Frame True TextFrame <$> arbitraryUtf8

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
    arbitrary = do
        fin <- arbitrary
        t <- arbitrary
        payload <- case t of
            TextFrame -> arbitraryUtf8
            _ -> BL.pack <$> arbitrary
        return $ Frame fin t payload

arbitraryUtf8 :: Gen BL.ByteString
arbitraryUtf8 = toLazyByteString . TL.encodeUtf8 . TL.pack <$> arbitrary

