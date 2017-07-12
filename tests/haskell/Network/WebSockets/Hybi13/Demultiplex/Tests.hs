--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Hybi13.Demultiplex.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                   ((<$>))
import qualified Data.ByteString.Lazy                  as BL
import           Network.WebSockets
import           Network.WebSockets.Hybi13.Demultiplex
import           Prelude
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.HUnit                            (Assertion, (@=?))


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Hybi13.Demultiplex.Tests"
    [ testMessageDataSizeLimit
    ]


--------------------------------------------------------------------------------
testMessageDataSizeLimit :: Test
testMessageDataSizeLimit = testGroup "testMessageDataSizeLimit Hybi13"
    [ testCase "OK 1" $
        Right [DataMessage False False False (Binary (mkZeroes 100))] @=?
        testDemultiplex (SizeLimit 100) (fragmented 5 20)
    , testCase "Exceeds 1" $
        assertLeft $
        testDemultiplex (SizeLimit 99) (fragmented 5 20)
    , testCase "Exceeds 2" $
        assertLeft $
        testDemultiplex (SizeLimit 100) (fragmented 6 20)
    , testCase "Exceeds 3" $
        assertLeft $
        testDemultiplex (SizeLimit 100) (fragmented 101 1)
    , testCase "Exceeds 4" $
        assertLeft $
        testDemultiplex (SizeLimit 100) (fragmented 1 101)
    ]
  where
    fragmented :: Int -> Int -> [Frame]
    fragmented n size =
        let payload = mkZeroes size in
        [Frame False False False False BinaryFrame payload] ++
        replicate (n - 2) (Frame False False False False ContinuationFrame payload) ++
        [Frame True False False False ContinuationFrame payload]

    mkZeroes :: Int -> BL.ByteString
    mkZeroes size = BL.replicate (fromIntegral size) 0

    assertLeft :: Either a b -> Assertion
    assertLeft (Left _)  = return ()
    assertLeft (Right _) = fail "Expecting test to fail"


--------------------------------------------------------------------------------
testDemultiplex
    :: SizeLimit
    -> [Frame]
    -> Either ConnectionException [Message]
testDemultiplex messageLimit = go emptyDemultiplexState
  where
    go _state0 []               = return []
    go state0  (frame : frames) = case demultiplex messageLimit state0 frame of
        (DemultiplexContinue, state1)  -> go state1 frames
        (DemultiplexError err, _)      -> Left err
        (DemultiplexSuccess m, state1) -> (m :) <$> go state1 frames
