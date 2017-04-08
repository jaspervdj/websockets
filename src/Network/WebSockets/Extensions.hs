module Network.WebSockets.Extensions
    ( ExtensionDescription (..)
    , ExtensionDescriptions

    , NegotiateExtension
    , Extension (..)
    ) where

import           Network.WebSockets.Extensions.Description
import           Network.WebSockets.Http
import           Network.WebSockets.Types

type NegotiateExtension = ExtensionDescriptions -> Either String Extension

-- | An extension is currently allowed to set extra headers and transform the
-- parse/write functions of 'Connection'.
--
-- This type is very likely to change as other extensions are introduced.
data Extension = Extension
    { extHeaders :: Headers
    , extParse   :: IO (Maybe Message) -> IO (IO (Maybe Message))
    , extWrite   :: ([Message] -> IO ()) -> IO ([Message] -> IO ())
    }
