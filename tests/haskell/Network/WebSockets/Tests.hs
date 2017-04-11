--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.WebSockets.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder              as Builder
import           Control.Applicative                   ((<$>))
import           Control.Concurrent                    (forkIO)
import           Control.Exception                     (try)
import           Control.Monad                         (replicateM)
import qualified Data.ByteString.Lazy                  as BL
import           Data.List                             (intersperse)
import           Data.Maybe                            (catMaybes)
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
import qualified Network.WebSockets.Stream             as Stream
import           Network.WebSockets.Tests.Util
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Test"
    [ testProperty "simple encode/decode Hybi13" (testSimpleEncodeDecode Hybi13)
    , testProperty "fragmented Hybi13"           testFragmentedHybi13
    ]


--------------------------------------------------------------------------------
testSimpleEncodeDecode :: Protocol -> Property
testSimpleEncodeDecode protocol = QC.monadicIO $
    QC.forAllM QC.arbitrary $ \msgs -> QC.run $ do
        echo  <- Stream.makeEchoStream
        parse <- decodeMessages protocol echo
        write <- encodeMessages protocol ClientConnection echo
        _     <- forkIO $ write msgs
        msgs' <- catMaybes <$> replicateM (length msgs) parse
        Stream.close echo
        msgs @=? msgs'


--------------------------------------------------------------------------------
testFragmentedHybi13 :: Property
testFragmentedHybi13 = QC.monadicIO $
    QC.forAllM QC.arbitrary $ \fragmented -> QC.run $ do
        echo     <- Stream.makeEchoStream
        parse    <- Hybi13.decodeMessages echo
        -- is'      <- Streams.filter isDataMessage =<< Hybi13.decodeMessages is

        -- Simple hacky encoding of all frames
        _ <- forkIO $ do
            mapM_ (Stream.write echo)
                [ Builder.toLazyByteString (Hybi13.encodeFrame Nothing f)
                | FragmentedMessage _ frames <- fragmented
                , f                          <- frames
                ]
            Stream.close echo

        -- Check if we got all data
        msgs <- filter isDataMessage <$> parseAll parse
        [msg | FragmentedMessage msg _ <- fragmented] @=? msgs
  where
    isDataMessage (ControlMessage _)    = False
    isDataMessage (DataMessage _ _ _ _) = True

    parseAll parse = do
        mbMsg <- try parse
        case mbMsg of
            Left  ConnectionClosed -> return []
            Left  _                -> return []
            Right (Just msg)       -> (msg :) <$> parseAll parse
            Right Nothing          -> return []


--------------------------------------------------------------------------------
instance Arbitrary Message where
    arbitrary = QC.oneof
        [ do
            payload <- BL.take 125 . BL.pack <$> arbitrary
            return $ ControlMessage (Ping payload)
        , do
            payload <- BL.take 125 . BL.pack <$> arbitrary
            return $ ControlMessage (Pong payload)
        , do
            payload <- BL.pack <$> arbitrary
            return $ DataMessage False False False (Text payload Nothing)
        , do
            payload <- BL.pack <$> arbitrary
            return $ DataMessage False False False (Binary payload)
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
                TextFrame   -> DataMessage False False False (Text payload Nothing)
                BinaryFrame -> DataMessage False False False (Binary payload)
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
            , Frame True False False False PongFrame "Derp"
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
