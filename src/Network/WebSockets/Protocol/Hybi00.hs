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
    version        (Hybi00 p) = version p
    headerVersions (Hybi00 p) = headerVersions p
    encodeFrame    (Hybi00 p) = encodeFrame p
    decodeFrame    (Hybi00 p) = decodeFrame p
    finishRequest  (Hybi00 p) = finishRequest p
    implementations           = [Hybi00 Hybi10_, Hybi00 Hybi00_]

instance TextProtocol Hybi00
