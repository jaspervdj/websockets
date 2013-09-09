--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                   (pure, (<$>))
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Concurrent.MVar               (newEmptyMVar, putMVar,
                                                        takeMVar)
import           Control.Exception                     (fromException)
import           Control.Monad                         (forM_, replicateM)
import           Control.Monad.Trans                   (liftIO)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Maybe                            (catMaybes)
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified Data.Text.Lazy                        as TL
import qualified System.IO.Streams                     as Streams
import qualified System.IO.Streams.Builder             as Streams
import qualified System.IO.Streams.List                as Streams
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.HUnit                            ((@=?))
import qualified Test.HUnit                            as HU
import           Test.QuickCheck                       (Arbitrary (..), Gen,
                                                        Property)
import qualified Test.QuickCheck                       as QC
import qualified Test.QuickCheck.Monadic               as QC


--------------------------------------------------------------------------------
import           Network.WebSockets
import           Network.WebSockets.Hybi13.Demultiplex
import           Network.WebSockets.Protocol
import           Network.WebSockets.Tests.Util
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "simple encode/decode Hybi13" (testSimpleEncodeDecode Hybi13)
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

{-
    [ testProperty "sendReceive-hybi10"       (sendReceive Hybi10_)
    , testProperty "sendReceive-hybi00"       (sendReceiveHybi00 Hybi00_)
    , testProperty "sendReceiveTextData-hybi10"
        (sendReceiveTextData Hybi10_)
    , testProperty "sendReceiveTextData-hybi00"
        (sendReceiveTextData Hybi00_)
    , testProperty "sendReceiveFragmented-hybi10" sendReceiveFragmentedHybi10
    , testProperty "sendReceiveClose-hybi10"  (sendReceiveClose Hybi10_)
    , testProperty "sendReceiveClose-hybi00"  (sendReceiveClose Hybi00_)
    , testCase     "pingThread-hybi10"        (pingThread Hybi10_)
    ]

sendReceiveFragmentedHybi10 :: FragmentedMessage Hybi10_ -> Property
sendReceiveFragmentedHybi10 (FragmentedMessage msg frames) = QC.monadicIO $ do
    -- Put some other frames in between
    let frames' = concatMap addCrap frames
        client  = mapM_ (sendBuilder . encodeFrameHybi10) frames'
    msg' <- QC.run $ pipe Hybi10_ client receiveDataMessage
    QC.assert $ msg == DataMessage msg'
  where
    addCrap x =
        [ Frame True False False False PingFrame "Herp"
        , Frame True True  True  True  PongFrame "Derp"
        , x
        ]

sendReceiveClose :: Protocol p => p -> Property
sendReceiveClose proto = QC.monadicIO $ do
    -- Put some other frames in between
    let msgs = [ControlMessage (Close ""), (DataMessage (Text "Herp"))]
    closed <- QC.run $ pipe proto (mapM_ send msgs) receiver
    QC.assert closed
  where
    receiver = catchWsError (receiveDataMessage >> return False) $ \e ->
        case fromException e of
            Just ConnectionClosed -> return True
            _                     -> return False
-}


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
        QC.elements
            [ ControlMessage (Close payload)
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
        ft        <- QC.elements [TextFrame, BinaryFrame]
        payload   <- arbitraryUtf8
        fragments <- arbitraryFragmentation payload
        let fs = makeFrames $ zip (ft : repeat ContinuationFrame) fragments
            msg = case ft of
                TextFrame   -> DataMessage (Text payload)
                BinaryFrame -> DataMessage (Binary payload)
                _           -> error "Arbitrary FragmentedMessage crashed"
        return $ FragmentedMessage msg fs
      where
        makeFrames []              = []
        makeFrames [(ft, pl)]      = [Frame True False False False ft pl]
        makeFrames ((ft, pl) : fr) =
            Frame False False False False ft pl : makeFrames fr


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
