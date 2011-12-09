{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests
    ( tests
    , ArbitraryUtf8 (..)
    ) where

import Control.Applicative (pure, (<$>))
import Control.Exception (fromException)
import Control.Monad (forM_, replicateM)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Set as S

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary (..), Gen, Property)
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Test.HUnit as HU
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import Network.WebSockets
import Network.WebSockets.Mask
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Tests.Util
import Network.WebSockets.Tests.Util.IterAccum
import qualified Network.WebSockets.Protocol.Unsafe as Unsafe

tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "encodeDecodeFrame-hybi10" (encodeDecodeFrame Hybi10_)
    , testProperty "encodeDecodeFrame-hybi00" (encodeDecodeFrameHybi00 Hybi00_)

    , testProperty "sendReceive-hybi10"       (sendReceive Hybi10_)
    , testProperty "sendReceive-hybi00"       (sendReceiveHybi00 Hybi00_)
    , testProperty "sendReceiveTextData-hybi10"
        (sendReceiveTextData Hybi10_)
    , testProperty "sendReceiveTextData-hybi00"
        (sendReceiveTextData Hybi00_)
    , testProperty "sendReceiveFragmented-hybi10"
        (sendReceiveFragmented Hybi10_)
    , testProperty "sendReceiveClose-hybi10"  (sendReceiveClose Hybi10_)
    , testProperty "sendReceiveClose-hybi00"  (sendReceiveClose Hybi00_)
    , testProperty "sendReceiveConcurrent-hybi10"
        (sendReceiveConcurrent Hybi10_)
    , testProperty "sendReceiveConcurrent-hybi00"
        (sendReceiveConcurrent Hybi00_)
    , testCase     "pingThread-hybi10"        (pingThread Hybi10_)
    ]

-- | Encode a frame, then decode it again. We should obviously get our original
-- frame back
encodeDecodeFrame :: Protocol p => p -> ArbitraryMask -> Frame -> Bool
encodeDecodeFrame proto (ArbitraryMask m) f =
    let lbs = Builder.toLazyByteString $ (encodeFrame proto) m f
        bs = B.concat $ BL.toChunks lbs
    in case A.parse (decodeFrame proto) bs of
        A.Done "" r -> f == r
        err         -> error ("encodeDecodeFrame: " ++ show err)

encodeDecodeFrameHybi00 :: Protocol p
                        => p -> ArbitraryMask -> ArbitraryFrameHybi00 -> Bool
encodeDecodeFrameHybi00 proto am (ArbitraryFrameHybi00 f) =
    encodeDecodeFrame proto am f

sendReceive :: Protocol p => p -> [Message p] -> Property
sendReceive proto msgs = QC.monadicIO $ do
    msgs' <- QC.run $ pipe proto (mapM_ send msgs) (receiver [])
    QC.assert $ msgs == msgs'
  where
    receiver msgs' = flip catchWsError (\_ -> return (reverse msgs')) $ do
        msg <- receive 
        receiver (msg : msgs')

sendReceiveHybi00 :: Protocol p
                  => p -> [ArbitraryMessageHybi00 p] -> Property
sendReceiveHybi00 proto = sendReceive proto . map unpack
  where
    unpack (ArbitraryMessageHybi00 msg) = msg

sendReceiveTextData :: TextProtocol p => p -> Property
sendReceiveTextData proto = QC.monadicIO $ do
    t <- T.pack <$> QC.pick arbitrary
    t' <- QC.run $ pipe proto (sendTextData t) receiveData
    QC.assert $ t == t'
    tl <- TL.pack <$> QC.pick arbitrary
    tl' <- QC.run $ pipe proto (sendTextData tl) receiveData
    QC.assert $ tl == tl'

sendReceiveFragmented :: Protocol p => p -> FragmentedMessage p -> Property
sendReceiveFragmented proto (FragmentedMessage msg frames) = QC.monadicIO $ do
    -- Put some other frames in between
    let frames' = concatMap addCrap frames
    msg' <- QC.run $ pipe proto (mapM_ sendFrame frames') receiveDataMessage
    QC.assert $ msg == DataMessage msg'
  where
    addCrap x = [Frame True PingFrame "Herp", Frame True PongFrame "Derp", x]

sendReceiveClose :: Protocol p => p -> Property
sendReceiveClose proto = QC.monadicIO $ do
    -- Put some other frames in between
    let frames = [Frame True CloseFrame "", Frame True TextFrame "Herp"]
    closed <- QC.run $ pipe proto (mapM_ sendFrame frames) receiver
    QC.assert closed
  where
    receiver = catchWsError (receiveDataMessage >> return False) $ \e ->
        case fromException e of
            Just ConnectionClosed -> return True
            _                     -> return False

sendReceiveConcurrent :: TextProtocol p => p -> Property
sendReceiveConcurrent proto = QC.monadicIO $ do
    -- Pick some text messages
    msgs <- QC.pick arbitrary
    msgs' <- QC.run $ pipe proto (sender msgs) (receiver [])
    return $ S.fromList msgs == S.fromList msgs'
  where
    sender msgs = do
        sink <- getSink
        locks <- liftIO $ mapM (const newEmptyMVar) msgs
        forM_ (zip msgs locks) $ \(ArbitraryUtf8 msg, lock) -> liftIO $ do
            _ <- forkIO $ do
                sendSink sink $ textData msg
                putMVar lock ()
            return ()
        liftIO $ forM_ locks takeMVar

    receiver msgs' = flip catchWsError (\_ -> return (reverse msgs')) $ do
        msg <- receiveData
        receiver (ArbitraryUtf8 msg : msgs')

pingThread :: BinaryProtocol p => p -> HU.Assertion
pingThread proto = HU.assert $ pipe proto server $ client 0
  where
    server   = spawnPingThread 1 >> liftIO (threadDelay $ 3 * 1000 * 1000)
    client 2 = return True
    client n = receive >>= \msg -> case msg of
        ControlMessage (Ping _) -> client (n + 1 :: Int)
        _                       -> return False

newtype ArbitraryMask = ArbitraryMask Mask
                      deriving (Show)

instance Arbitrary ArbitraryMask where
    arbitrary = ArbitraryMask <$> QC.oneof
        [ return Nothing
        , Just . B.pack <$> replicateM 4 arbitrary
        ]

instance Arbitrary FrameType where
    arbitrary = QC.elements
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

instance (TextProtocol p, BinaryProtocol p) => Arbitrary (Message p) where
    arbitrary = do
        payload <- BL.pack <$> arbitrary
        QC.elements
            [ close      payload
            , ping       payload
            , pong       payload
            , textData   payload
            , binaryData payload
            ]

newtype ArbitraryMessageHybi00 p = ArbitraryMessageHybi00 (Message p)
    deriving (Show)

instance Arbitrary (ArbitraryMessageHybi00 p) where
    arbitrary = ArbitraryMessageHybi00 <$> QC.oneof
        [ ControlMessage . Close <$> pure ""
        , DataMessage    . Text  <$> arbitraryUtf8
        ]

data FragmentedMessage p
    = FragmentedMessage (Message p) [Frame]
    deriving (Show)

instance Arbitrary (FragmentedMessage p) where
    arbitrary = do
        ft <- QC.elements [TextFrame, BinaryFrame]
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
