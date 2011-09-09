module Network.WebSockets.Encode
    ( Encoder
    , frame
    ) where

import Data.Monoid (mappend)
import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Word (fromWord8)
import Blaze.ByteString.Builder.ByteString (copyByteString)

import Network.WebSockets.Types

type Encoder a = a -> Builder

frame :: Encoder Frame
frame bs = fromWord8 0 `mappend` copyByteString bs `mappend` fromWord8 0xff
