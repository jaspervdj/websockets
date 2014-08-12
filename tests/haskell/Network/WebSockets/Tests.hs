--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder              as Builder
import           Control.Applicative                   ((<$>))
import           Control.Monad                         (replicateM)
import qualified Data.ByteString.Lazy                  as BL
import           Data.List                             (intersperse)
import           Data.Maybe                            (catMaybes)
import qualified System.IO.Streams                     as Streams
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.HUnit                            ((@=?))
import           Test.QuickCheck                       (Arbitrary (..), Gen,
                                                        Property)
import qualified Test.QuickCheck                       as QC
import qualified Test.QuickCheck.Monadic               as QC


--------------------------------------------------------------------------------
import           Network.WebSockets
import qualified Network.WebSockets.Hybi13             as Hybi13
import           Network.WebSockets.Hybi13.Demultiplex
import           Network.WebSockets.Protocol
import           Network.WebSockets.Tests.Util
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "simple encode/decode Hybi13" (testSimpleEncodeDecode Hybi13)
    , testProperty "framgmented Hybi13"          testFragmentedHybi13
    ]


--------------------------------------------------------------------------------
testSimpleEncodeDecode :: Protocol -> Property
testSimpleEncodeDecode protocol = QC.monadicIO $
    QC.forAllM QC.arbitrary $ \msgs -> QC.run $ do
        (is, os) <- makeChanPipe
        is'      <- decodeMessages protocol is
        os'      <- encodeMessages protocol ClientConnection =<<
            Streams.builderStream os
        Streams.writeList msgs os'
        msgs' <- catMaybes <$> replicateM (length msgs) (Streams.read is')
        msgs @=? msgs'


--------------------------------------------------------------------------------
testFragmentedHybi13 :: Property
testFragmentedHybi13 = QC.monadicIO $
    QC.forAllM QC.arbitrary $ \fragmented -> QC.run $ do
        (is, os) <- makeChanPipe
        is'      <- Streams.filter isDataMessage =<< Hybi13.decodeMessages is
        os'      <- Streams.builderStream os

        -- Simple hacky encoding of all frames
        Streams.writeList
            [ Hybi13.encodeFrame Nothing f
            | FragmentedMessage _ frames <- fragmented
            , f                          <- frames
            ] os'
        Streams.write (Just Builder.flush) os'
        Streams.write Nothing os'

        -- Check if we got all data
        msgs <- catMaybes <$> replicateM (length fragmented) (Streams.read is')
        [msg | FragmentedMessage msg _ <- fragmented] @=? msgs
  where
    isDataMessage (ControlMessage _) = False
    isDataMessage (DataMessage _)    = True


--------------------------------------------------------------------------------
instance Arbitrary FrameType where
    arbitrary = QC.elements
        [ ContinuationFrame
        , TextFrame
        , BinaryFrame
        , CloseFrame
        , PingFrame
        , PongFrame
        ]


--------------------------------------------------------------------------------
instance Arbitrary Frame where
    arbitrary = do
        fin  <- arbitrary
        rsv1 <- arbitrary
        rsv2 <- arbitrary
        rsv3 <- arbitrary
        t    <- arbitrary
        payload <- case t of
            TextFrame -> arbitraryUtf8
            _         -> BL.pack <$> arbitrary
        return $ Frame fin rsv1 rsv2 rsv3 t payload


--------------------------------------------------------------------------------
instance Arbitrary Message where
    arbitrary = do
        payload <- BL.pack <$> arbitrary
        closecode <- arbitrary
        QC.elements
            [ ControlMessage (Close closecode payload)
            , ControlMessage (Ping payload)
            , ControlMessage (Pong payload)
            , DataMessage (Text payload)
            , DataMessage (Binary payload)
            ]


--------------------------------------------------------------------------------
data FragmentedMessage = FragmentedMessage Message [Frame]
    deriving (Show)


--------------------------------------------------------------------------------
instance Arbitrary FragmentedMessage where
    arbitrary = do
        -- Pick a frametype and a corresponding random payload
        ft        <- QC.elements [TextFrame, BinaryFrame]
        payload   <- case ft of
            TextFrame -> arbitraryUtf8
            _         -> arbitraryByteString

        fragments <- arbitraryFragmentation payload
        let fs  = makeFrames $ zip (ft : repeat ContinuationFrame) fragments
            msg = case ft of
                TextFrame   -> DataMessage (Text payload)
                BinaryFrame -> DataMessage (Binary payload)
                _           -> error "Arbitrary FragmentedMessage crashed"

        interleaved <- arbitraryInterleave genControlFrame fs
        return $ FragmentedMessage msg interleaved
        -- return $ FragmentedMessage msg fs
      where
        makeFrames []              = []
        makeFrames [(ft, pl)]      = [Frame True False False False ft pl]
        makeFrames ((ft, pl) : fr) =
            Frame False False False False ft pl : makeFrames fr

        genControlFrame = QC.elements
            [ Frame True False False False PingFrame "Herp"
            , Frame True True  True  True  PongFrame "Derp"
            ]


--------------------------------------------------------------------------------
arbitraryFragmentation :: BL.ByteString -> Gen [BL.ByteString]
arbitraryFragmentation bs = arbitraryFragmentation' bs
  where
    len :: Int
    len = fromIntegral $ BL.length bs
    arbitraryFragmentation' bs' = do
        -- TODO: we currently can't send packets of length 0. We should
        -- investigate why (regardless of the spec).
        n <- QC.choose (1, len - 1)
        let (l, r) = BL.splitAt (fromIntegral n) bs'
        case r of
            "" -> return [l]
            _  -> (l :) <$> arbitraryFragmentation' r


--------------------------------------------------------------------------------
arbitraryInterleave :: Gen a -> [a] -> Gen [a]
arbitraryInterleave sep xs = fmap concat $ sequence $
    [sep'] ++ intersperse sep' [return [x] | x <- xs] ++ [sep']
  where
    sep' = QC.sized $ \size -> do
        num <- QC.choose (1, size)
        replicateM num sep
