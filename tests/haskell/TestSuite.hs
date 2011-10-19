import Test.Framework (defaultMain)

import qualified Network.WebSockets.Tests
import qualified Network.WebSockets.Handshake.Tests

main :: IO ()
main = defaultMain
    [ Network.WebSockets.Tests.tests
    , Network.WebSockets.Handshake.Tests.tests
    ]
