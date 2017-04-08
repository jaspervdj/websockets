--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Extensions.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Network.WebSockets.Extensions
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@?=))


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Network.WebSockets.Extensions.Tests"
    [ testCase "parseExtensionDescriptions 01" $ do
        parseExtensionDescriptions "permessage-deflate" @?= Right
            [ ExtensionDescription "permessage-deflate" [] ]

    , testCase "parseExtensionDescriptions 02" $ do
        parseExtensionDescriptions "permessage-deflate; client_max_window_bits; server_max_window_bits=10" @?= Right
            [ ExtensionDescription "permessage-deflate"
                [ ("client_max_window_bits", Nothing)
                , ("server_max_window_bits", Just "10")
                ]
            ]

    , testCase "parseExtensionDescriptions 03" $ do
        parseExtensionDescriptions "permessage-deflate; client_max_window_bits=15; server_max_window_bits=10, permessage-deflate; client_max_window_bits,permessage-deflate; client_max_window_bits=15; client_max_window_bits=10" @?= Right
            [ ExtensionDescription "permessage-deflate"
                [ ("client_max_window_bits", Just "15")
                , ("server_max_window_bits", Just "10")
                ]
            , ExtensionDescription "permessage-deflate"
                [ ("client_max_window_bits", Nothing)
                ]
            , ExtensionDescription "permessage-deflate"
                [ ("client_max_window_bits", Just "15")
                , ("client_max_window_bits", Just "10")
                ]
            ]
    ]
