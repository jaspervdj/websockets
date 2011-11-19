{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Network.WebSockets.Protocol.Hybi00
       ( Hybi00_ (..)
       , Hybi00
       ) where

import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal

data Hybi00 = forall p. Protocol p => Hybi00 p

instance Protocol Hybi00 where
    version       (Hybi00 p) = version p
    headerVersion (Hybi00 p) = headerVersion p
    encodeFrame   (Hybi00 p) = encodeFrame p
    decodeFrame   (Hybi00 p) = decodeFrame p
    finishRequest (Hybi00 p) = finishRequest p
    implementations          = [Hybi00 Hybi00_, Hybi00 Hybi10_]

instance TextProtocol Hybi00
