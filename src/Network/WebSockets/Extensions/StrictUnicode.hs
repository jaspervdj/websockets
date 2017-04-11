--------------------------------------------------------------------------------
module Network.WebSockets.Extensions.StrictUnicode
    ( strictUnicode
    ) where


--------------------------------------------------------------------------------
import           Control.Exception                         (throwIO)
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
strictParse (Just (DataMessage rsv1 rsv2 rsv3 (Text bl _))) = do
    txt <- decodeUtf8Strict bl >>= either throwIO return
    return (Just (DataMessage rsv1 rsv2 rsv3 (Text bl (Just txt))))
strictParse (Just msg) = return (Just msg)
