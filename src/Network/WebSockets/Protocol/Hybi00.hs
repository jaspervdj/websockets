{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Protocol.Hybi00
       ( hybi00
       ) where

import Control.Applicative
import Control.Monad.Error (throwError, ErrorT (..))
import Control.Monad.Trans (lift)

import Data.Char (isDigit, chr, ord)

import Data.Digest.Pure.MD5 (md5)
import Data.Bits (shift)
import Data.Word (Word8)
import Data.Binary (encode)
import qualified Blaze.ByteString.Builder as BB
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec as A

import Network.WebSockets.Encode (Encoder)
import Network.WebSockets.Types


encodeFrameHybi00 :: Encoder Frame
encodeFrameHybi00 _ (Frame True _ pl) =
  BB.fromLazyByteString $ "\0" `BL.append` pl `BL.append` "\255"
-- TODO: otherwise, fail in a sane way (and not with non-exhaustive pattern
-- match crash) OTOH, we *should* fail when trying to send binary.
-- TODO: Can't we send a close frame ourselves? Also, how do we do a close
-- handshake for hybi10? (I guess we don't, at the moment)

decodeFrameHybi00 :: Decoder Frame
decodeFrameHybi00 = decodeTextFrame <|> decodeCloseFrame

decodeTextFrame = do
  _ <- A.word8 $ fromIntegral 0
  utf8string <- A.manyTill A.anyWord8 (A.try . A.word8 . fromIntegral $ 255)
  return $ Frame True TextFrame $ BL.pack utf8string

decodeCloseFrame = do
  _ <- A.word8 $ fromIntegral 255
  _ <- A.word8 $ fromIntegral 0
  return $ Frame True CloseFrame ""

divBySpaces :: String -> Int
divBySpaces str =
  let number = read $ filter isDigit str :: Integer
      spaces = fromIntegral . length $ filter (==' ') str
  in  fromIntegral $ number `div` spaces
-- TODO: what if str has 0 spaces?


handshakeHybi00 :: RequestHttpPart
                   -> ErrorT HandshakeError A.Parser Request
handshakeHybi00 reqHttp@(RequestHttpPart path h) = do
  -- todo: order correct? (spec, p.22, 28.). Require at least two 0x20!
  _ <- lift . A.word8 $ fromIntegral 0x0d
  _ <- lift . A.word8 $ fromIntegral 0x0a
  keyPart3 <- lift $ A.take 8
  keyPart1 <- B.pack . toWord8List . divBySpaces . BC.unpack <$>
              getHeader "Sec-WebSocket-Key1"
  keyPart2 <- B.pack . toWord8List . divBySpaces . BC.unpack <$>
              getHeader "Sec-WebSocket-Key2"
  let key = B.concat . BL.toChunks . encode . md5 $
            BL.fromChunks [keyPart1 `BC.append` keyPart2 `BC.append` keyPart3]
  host <- getHeader "Host"
  -- todo: origin right? (also applies to hybi10)
  origin <- getHeader "Origin"
  let response = Response 101 "WebSocket Protocol Handshake"
                 (("Upgrade", "WebSocket") :
                  ("Connection", "Upgrade") :
                  ("Sec-WebSocket-Location",
                   "ws://" `B.append` host `B.append` path) :
                  ("Sec-WebSocket-Origin", origin) : []) $
                 "\r\n" `B.append` key
  return $ Request path h response
    where
      getHeader k = case lookup k h of
        Just t  -> return t
        Nothing -> throwError $
                   MalformedRequest reqHttp $ "Header missing: " ++ BC.unpack (CI.original k)
      toWord8List n =
        map (\k -> (fromIntegral (shift n k)) :: Word8) [24,16,8,0]

hybi00 :: Protocol
hybi00 = Protocol "hybi00" encodeFrameHybi00 decodeFrameHybi00 $ runErrorT . handshakeHybi00
