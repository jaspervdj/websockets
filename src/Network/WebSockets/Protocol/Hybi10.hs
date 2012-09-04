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
    version          (Hybi10 p)   = version p
    headerVersions   (Hybi10 p)   = headerVersions p
    supported        (Hybi10 p) h = supported p h
    encodeMessages   (Hybi10 p)   = (EL.map castMessage =$) . encodeMessages p
    decodeMessages   (Hybi10 p)   = (decodeMessages p =$) . EL.map castMessage
    createRequest    (Hybi10 p)   = createRequest p
    finishRequest    (Hybi10 p)   = finishRequest p
    responseSize     (Hybi10 p)   = responseSize p
    validateResponse (Hybi10 p)   = validateResponse p
    implementations               = [Hybi10 Hybi10_]

instance TextProtocol Hybi10
instance BinaryProtocol Hybi10
