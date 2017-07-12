--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Extensions.PermessageDeflate.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Control.Exception                               (try)
import qualified Data.ByteString.Lazy                            as BL
import           Network.WebSockets.Extensions.PermessageDeflate
import           Network.WebSockets.Types
import           Network.WebSockets.Connection.Options
import           Test.Framework                                  (Test,
                                                                  testGroup)
import           Test.Framework.Providers.HUnit                  (testCase)
import           Test.HUnit                                      (Assertion,
                                                                  (@?=))


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Extensions.PermessageDeflate.Tests"
    [ testCase "OK 1" $ do
        inflater <- makeMessageInflater
            (SizeLimit 100) (Just defaultPermessageDeflate)
        message <- inflater $ DataMessage True False False (Binary deflated100)
        message @?=
            DataMessage False False False (Binary inflated100)
    , testCase "Exceed 1" $ do
        inflater <- makeMessageInflater
            (SizeLimit 99) (Just defaultPermessageDeflate)
        assertParseException $
            inflater $ DataMessage True False False (Binary deflated100)
    ]
  where
    assertParseException :: IO a -> Assertion
    assertParseException io = do
        errOrX <- try io
        case errOrX of
            Left (ParseException _) -> return ()
            _                       -> fail "Excepted ParseException"

    -- This inflates to 100 bytes.
    deflated100 = "b`\160=\NUL\NUL"
    inflated100 = BL.replicate 100 0
