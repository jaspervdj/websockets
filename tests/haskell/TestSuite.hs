--------------------------------------------------------------------------------
import           Test.Framework                     (defaultMain)


--------------------------------------------------------------------------------
import qualified Network.WebSockets.Handshake.Tests
import qualified Network.WebSockets.Http.Tests
import qualified Network.WebSockets.Server.Tests
import qualified Network.WebSockets.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ Network.WebSockets.Handshake.Tests.tests
    , Network.WebSockets.Http.Tests.tests
    , Network.WebSockets.Server.Tests.tests
    , Network.WebSockets.Tests.tests
    ]
