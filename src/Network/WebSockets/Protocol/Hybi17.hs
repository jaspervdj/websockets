{-# LANGUAGE ExistentialQuantification #-}
module Network.WebSockets.Protocol.Hybi17
    ( Hybi17
    ) where

import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Protocol.Hybi17.Internal

data Hybi17 = forall p. Protocol p => Hybi17 p

instance Protocol Hybi17 where
    version       (Hybi17 p) = version p
    headerVersion (Hybi17 p) = headerVersion p
    encodeFrame   (Hybi17 p) = encodeFrame p
    decodeFrame   (Hybi17 p) = decodeFrame p
    finishRequest (Hybi17 p) = finishRequest p
    implementations          = [Hybi17 Hybi17_, Hybi17 Hybi10_]

instance TextProtocol Hybi17
instance BinaryProtocol Hybi17
