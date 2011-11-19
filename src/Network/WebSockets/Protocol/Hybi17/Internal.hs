{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Protocol.Hybi17.Internal
    ( Hybi17_ (..)
    ) where

import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi10.Internal

data Hybi17_ = Hybi17_

instance Protocol Hybi17_ where
    version         Hybi17_ = "hybi17"
    headerVersion   Hybi17_ = "13"
    encodeFrame     Hybi17_ = encodeFrame   Hybi10_
    decodeFrame     Hybi17_ = decodeFrame   Hybi10_
    finishRequest   Hybi17_ = finishRequest Hybi10_
    implementations         = [Hybi17_]

instance TextProtocol Hybi17_
instance BinaryProtocol Hybi17_
