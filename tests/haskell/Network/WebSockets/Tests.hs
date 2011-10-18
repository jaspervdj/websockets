{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Control.Monad.Trans (liftIO)

import Data.Attoparsec (Result (..), parse)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, elements, oneof)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Enumerator as E
import Data.Enumerator (($$))

import Network.WebSockets
import Network.WebSockets.Mask
import Network.WebSockets.Monad
import Network.WebSockets.Protocol (Protocol (..))
import Network.WebSockets.Protocol.Hybi00 (Hybi00_ (..))
import Network.WebSockets.Protocol.Hybi10 (Hybi10_ (..))
import Network.WebSockets.Types
import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Encode as E
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe

tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "encodeFrameDecodeFrame-hybi10" (encodeFrameDecodeFrame Hybi10_)
    , testProperty "encodeFrameDecodeFrame-hybi00" (encodeFrameDecodeFrameFor00 Hybi00_)
    , testProperty "fragmententation-hybi10" (fragmentation Hybi10_)
    ]

-- | Encode a frame, then decode it again. We should obviously get our original
-- frame back
encodeFrameDecodeFrame :: Protocol p => p -> ArbitraryMask -> Frame -> Bool
encodeFrameDecodeFrame proto (ArbitraryMask m) f =
    let lbs = Builder.toLazyByteString $ (encodeFrame proto) m f
        bs = B.concat $ BL.toChunks lbs
    in case parse (decodeFrame proto) bs of
        Done "" r -> f == r
        err       -> error ("encodeFrameDecodeFrame: " ++ show err)

encodeFrameDecodeFrameFor00 :: Protocol p => p -> ArbitraryMask -> ArbitraryFrameFor00 -> Bool
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

fragmentation :: Protocol p => p -> Property
fragmentation proto = monadicIO $ do
    FragmentedMessage msg fragments <- pick arbitrary
    builders <- run $ mapM encodeFrame' fragments
    let bss = concatMap (BL.toChunks . Builder.toLazyByteString) builders
    msg' <- run $ E.run_ $ E.enumList 10 bss $$
        runWebSocketsWith' defaultWebSocketsOptions proto app out
    assert $ msg == msg'
  where
    app = receive
    out = return ()
    encodeFrame' fr = do
        mask <- randomMask
        return $ encodeFrame proto mask fr

data FragmentedMessage p
    = FragmentedMessage (Message p) [Frame]
    deriving (Show)

instance Arbitrary (FragmentedMessage p) where
    arbitrary = do
        ft <- elements [TextFrame, BinaryFrame]
        payload <- arbitraryUtf8
        fragments <- arbitraryFragmentation payload
        let fs = makeFrames $ zip (ft : repeat ContinuationFrame) fragments
            msg = case ft of
                TextFrame   -> Unsafe.textData payload
                BinaryFrame -> Unsafe.binaryData payload
                _           -> error "Arbitrary FragmentedMessage crashed"
        return $ FragmentedMessage msg fs
      where
        makeFrames []              = []
        makeFrames [(ft, pl)]      = [Frame True ft pl]
        makeFrames ((ft, pl) : fr) = Frame False ft pl : makeFrames fr

arbitraryUtf8 :: Gen BL.ByteString
arbitraryUtf8 = toLazyByteString . TL.encodeUtf8 . TL.pack <$> arbitrary

arbitraryFragmentation :: BL.ByteString -> Gen [BL.ByteString]
arbitraryFragmentation bs = arbitraryFragmentation' bs
  where
    len :: Int
    len = fromIntegral $ BL.length bs
    arbitraryFragmentation' bs' = do
        -- TODO: we currently can't send packets of length 0. We should
        -- investigate why (regardless of the spec).
        n <- choose (1, len - 1)
        let (l, r) = BL.splitAt (fromIntegral n) bs'
        case r of
            "" -> return [l]
            _  -> (l :) <$> arbitraryFragmentation' r
