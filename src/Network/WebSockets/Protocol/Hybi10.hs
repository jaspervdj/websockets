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
    encodeFrame    (Hybi10 p) = encodeFrame p
    enumMessages   (Hybi10 p) = (enumMessages p =$) . EL.map castMessage
    finishRequest  (Hybi10 p) = finishRequest p
    implementations           = [Hybi10 Hybi10_]

instance TextProtocol Hybi10
instance BinaryProtocol Hybi10
