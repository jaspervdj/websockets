import Test.Framework (defaultMain)

import qualified Network.WebSockets.Tests
import qualified Network.WebSockets.Handshake.Http.Tests
import qualified Network.WebSockets.Handshake.Tests
import qualified Network.WebSockets.Socket.Tests

main :: IO ()
main = defaultMain
    [ Network.WebSockets.Tests.tests
    , Network.WebSockets.Handshake.Http.Tests.tests
    , Network.WebSockets.Handshake.Tests.tests
    , Network.WebSockets.Socket.Tests.tests
    ]
