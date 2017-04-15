--------------------------------------------------------------------------------
module Network.WebSockets.Extensions.StrictUnicode
    ( strictUnicode
    ) where


--------------------------------------------------------------------------------
import           Control.Exception             (throwIO)
import qualified Data.ByteString.Lazy          as BL
import           Network.WebSockets.Extensions
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
strictUnicode :: Extension
strictUnicode = Extension
    { extHeaders = []
    , extParse   = \parseRaw -> return (parseRaw >>= strictParse)
    , extWrite   = return
    }


--------------------------------------------------------------------------------
strictParse :: Maybe Message -> IO (Maybe Message)
strictParse Nothing = return Nothing
strictParse (Just (DataMessage rsv1 rsv2 rsv3 (Text bl _))) =
    case decodeUtf8Strict bl of
        Left err   -> throwIO err
        Right txt ->
            return (Just (DataMessage rsv1 rsv2 rsv3 (Text bl (Just txt))))
strictParse (Just msg@(ControlMessage (Close _ bl))) =
    -- If there is a body, the first two bytes of the body MUST be a 2-byte
    -- unsigned integer (in network byte order) representing a status code with
    -- value /code/ defined in Section 7.4.  Following the 2-byte integer, the
    -- body MAY contain UTF-8-encoded data with value /reason/, the
    -- interpretation of which is not defined by this specification.
    case decodeUtf8Strict (BL.drop 2 bl) of
        Left err -> throwIO err
        Right _  -> return (Just msg)
strictParse (Just msg) = return (Just msg)
