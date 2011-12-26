{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Network.WebSockets.Protocol.Hybi00
       ( Hybi00_ (..)
       , Hybi00
       ) where

import Data.Enumerator ((=$))
import Data.Enumerator.List as EL
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi00.Internal
import Network.WebSockets.Protocol.Hybi10.Internal
import Network.WebSockets.Protocol.Unsafe

data Hybi00 = forall p. Protocol p => Hybi00 p

instance Protocol Hybi00 where
    version        (Hybi00 p)   = version p
    headerVersions (Hybi00 p)   = headerVersions p
    encodeMessages (Hybi00 p) g = (EL.map castMessage =$) . encodeMessages p g
    decodeMessages (Hybi00 p)   = (decodeMessages p =$) . EL.map castMessage
    finishRequest  (Hybi00 p)   = finishRequest p
    implementations             = [Hybi00 Hybi10_, Hybi00 Hybi00_]

instance TextProtocol Hybi00
