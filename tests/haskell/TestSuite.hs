--------------------------------------------------------------------------------
import qualified Network.WebSockets.Extensions.Tests
import qualified Network.WebSockets.Handshake.Tests
import qualified Network.WebSockets.Http.Tests
import qualified Network.WebSockets.Mask.Tests
import qualified Network.WebSockets.Server.Tests
import qualified Network.WebSockets.Tests
import           Test.Framework                      (defaultMain)


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ Network.WebSockets.Extensions.Tests.tests
    , Network.WebSockets.Handshake.Tests.tests
    , Network.WebSockets.Http.Tests.tests
    , Network.WebSockets.Server.Tests.tests
    , Network.WebSockets.Mask.Tests.tests
    , Network.WebSockets.Tests.tests
    ]
