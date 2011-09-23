import Test.Framework (defaultMain)

import qualified Network.WebSockets.Tests

main :: IO ()
main = defaultMain
    [ Network.WebSockets.Tests.tests
    ]
