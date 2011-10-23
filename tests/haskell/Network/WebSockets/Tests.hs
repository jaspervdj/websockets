{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests
    ( tests
    ) where

import Control.Applicative (pure, (<$>))
import Control.Exception (fromException)
import Control.Monad (forM_, replicateM)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.List (intercalate)
import qualified Data.Set as S

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

    , testProperty "sendReceive-hybi10"       (sendReceive Hybi10_)
    , testProperty "sendReceive-hybi00"       (sendReceiveHybi00 Hybi00_)
    , testProperty "sendReceiveFragmented-hybi10"
        (sendReceiveFragmented Hybi10_)
    , testProperty "sendReceiveClose-hybi10"  (sendReceiveClose Hybi10_)
    , testProperty "sendReceiveClose-hybi00"  (sendReceiveClose Hybi00_)
    , testProperty "sendReceiveConcurrent-hybi10"
        (sendReceiveConcurrent Hybi10_)
    , testProperty "sendReceiveConcurrent-hybi00"
        (sendReceiveConcurrent Hybi00_)
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

sendReceive :: Protocol p => p -> [Message p] -> Property
sendReceive proto msgs = monadicIO $ do
    msgs' <- run $ pipe proto (mapM_ send msgs) (receiver [])
    assert $ msgs == msgs'
  where
    receiver msgs' = flip catchWsError (\_ -> return (reverse msgs')) $ do
        msg <- receive 
        receiver (msg : msgs')

sendReceiveHybi00 :: Protocol p
                  => p -> [ArbitraryMessageHybi00 p] -> Property
sendReceiveHybi00 proto = sendReceive proto . map unpack
  where
    unpack (ArbitraryMessageHybi00 msg) = msg

sendReceiveFragmented :: Protocol p => p -> FragmentedMessage p -> Property
sendReceiveFragmented proto (FragmentedMessage msg frames) = monadicIO $ do
    -- Put some other frames in between
    let frames' = concatMap addCrap frames
    msg' <- run $ pipe proto (mapM_ sendFrame frames') receiveDataMessage
    assert $ msg == DataMessage msg'
  where
    addCrap x = [Frame True PingFrame "Herp", Frame True PongFrame "Derp", x]

sendReceiveClose :: Protocol p => p -> Property
sendReceiveClose proto = monadicIO $ do
    -- Put some other frames in between
    let frames = [Frame True CloseFrame "", Frame True TextFrame "Herp"]
    closed <- run $ pipe proto (mapM_ sendFrame frames) receiver
    assert closed
  where
    receiver = catchWsError (receiveDataMessage >> return False) $ \e ->
        case fromException e of
            Just ConnectionClosed -> return True
            _                     -> return False

sendReceiveConcurrent :: TextProtocol p => p -> Property
sendReceiveConcurrent proto = monadicIO $ do
    -- Pick some text messages
    msgs <- pick arbitrary
    msgs' <- run $ pipe proto (sender msgs) (receiver [])
    run $ print msgs >> print msgs'
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

instance TextProtocol Hybi00_
instance TextProtocol Hybi10_

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

newtype ArbitraryUtf8 = ArbitraryUtf8 BL.ByteString
    deriving (Eq, Ord, Show)

instance Arbitrary ArbitraryUtf8 where
    arbitrary = ArbitraryUtf8 <$> arbitraryUtf8

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
