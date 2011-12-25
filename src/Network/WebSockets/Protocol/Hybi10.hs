{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol.Hybi10
    ( Hybi10
    ) where

import Data.Enumerator ((=$))
import Data.Enumerator.List as EL
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Protocol.Unsafe

data Hybi10 = forall p. Protocol p => Hybi10 p

instance Protocol Hybi10 where
    version        (Hybi10 p) = version p
    headerVersions (Hybi10 p) = headerVersions p
    encodeMessage  (Hybi10 p) = \f -> encodeMessage p f . castMessage
    decodeMessages (Hybi10 p) = (decodeMessages p =$) . EL.map castMessage
    finishRequest  (Hybi10 p) = finishRequest p
    implementations           = [Hybi10 Hybi10_]

instance TextProtocol Hybi10
instance BinaryProtocol Hybi10
