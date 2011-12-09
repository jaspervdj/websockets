{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol.Hybi10
    ( Hybi10
    ) where

import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi10.Internal

data Hybi10 = forall p. Protocol p => Hybi10 p

instance Protocol Hybi10 where
    version        (Hybi10 p) = version p
    headerVersions (Hybi10 p) = headerVersions p
    encodeFrame    (Hybi10 p) = encodeFrame p
    decodeFrame    (Hybi10 p) = decodeFrame p
    finishRequest  (Hybi10 p) = finishRequest p
    implementations           = [Hybi10 Hybi10_]

instance TextProtocol Hybi10
instance BinaryProtocol Hybi10
