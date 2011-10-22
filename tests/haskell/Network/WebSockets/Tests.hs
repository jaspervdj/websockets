{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests
    ( tests
    ) where

import Control.Applicative (pure, (<$>))
import Control.Monad (replicateM)

import Data.Attoparsec (Result (..), parse)
import Data.Enumerator (($$))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, elements, oneof)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Enumerator as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.WebSockets
import Network.WebSockets.Mask
import Network.WebSockets.Monad
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi00
import Network.WebSockets.Protocol.Hybi10
import Network.WebSockets.Tests.Util.IterAccum
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe

tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "encodeDecodeFrame-hybi10" (encodeDecodeFrame Hybi10_)
    , testProperty "encodeDecodeFrame-hybi00" (encodeDecodeFrameHybi00 Hybi00_)
    , testProperty "fragmententation-hybi10"  (fragmentation Hybi10_)
    , testProperty "pipeSendReceive-hybi10"   (pipeSendReceive Hybi10_)
    , testProperty "pipeSendReceive-hybi00"   (pipeSendReceiveHybi00 Hybi00_)
    ]

-- | Encode a frame, then decode it again. We should obviously get our original
-- frame back
encodeDecodeFrame :: Protocol p => p -> ArbitraryMask -> Frame -> Bool
encodeDecodeFrame proto (ArbitraryMask m) f =
    let lbs = Builder.toLazyByteString $ (encodeFrame proto) m f
        bs = B.concat $ BL.toChunks lbs
    in case parse (decodeFrame proto) bs of
        Done "" r -> f == r
        err       -> error ("encodeDecodeFrame: " ++ show err)

encodeDecodeFrameHybi00 :: Protocol p
                        => p -> ArbitraryMask -> ArbitraryFrameHybi00 -> Bool
encodeDecodeFrameHybi00 proto am (ArbitraryFrameHybi00 f) =
    encodeDecodeFrame proto am f

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

pipeSendReceive :: Protocol p => p -> [Message p] -> Property
pipeSendReceive proto msgs = monadicIO $ do
    msgs' <- run $ pipe proto (mapM_ send msgs) (receiver [])
    assert $ msgs == msgs'
  where
    receiver msgs' = flip catchWsError (\_ -> return (reverse msgs')) $ do
        msg <- receive 
        receiver (msg : msgs')

pipeSendReceiveHybi00 :: Protocol p
                      => p -> [ArbitraryMessageHybi00 p] -> Property
pipeSendReceiveHybi00 proto = pipeSendReceive proto . map unpack
  where
    unpack (ArbitraryMessageHybi00 msg) = msg

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

newtype ArbitraryFrameHybi00 = ArbitraryFrameHybi00 Frame
    deriving (Show)

instance Arbitrary ArbitraryFrameHybi00 where
    arbitrary = ArbitraryFrameHybi00 <$>
        Frame True TextFrame <$> arbitraryUtf8

instance Arbitrary (Message p) where
    arbitrary = do
        payload <- BL.pack <$> arbitrary
        oneof
            [ ControlMessage <$> elements
                [ Close payload
                , Ping  payload
                , Pong  payload
                ]
            , DataMessage <$> elements
                [ Binary payload
                , Text   payload
                ]
            ]

newtype ArbitraryMessageHybi00 p = ArbitraryMessageHybi00 (Message p)
    deriving (Show)

instance Arbitrary (ArbitraryMessageHybi00 p) where
    arbitrary = ArbitraryMessageHybi00 <$> oneof
        [ ControlMessage . Close <$> pure ""
        , DataMessage    . Text  <$> arbitraryUtf8
        ]

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
