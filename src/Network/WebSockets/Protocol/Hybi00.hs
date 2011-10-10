{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Network.WebSockets.Protocol.Hybi00
       ( Hybi00 (..)
       , hybi00Protocols
       ) where

import Control.Applicative
import Control.Monad.Error (throwError, ErrorT (..))
import Control.Monad.Trans (lift)

import Data.Char (isDigit, chr, ord)

import Data.Digest.Pure.MD5 (md5)
import Data.Bits (shift)
import Data.Word (Word8)
import Data.Binary (encode)
import Data.Int (Int32)
import qualified Blaze.ByteString.Builder as BB
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Attoparsec as A

import Network.WebSockets.Encode (Encoder)
import Network.WebSockets.Protocol
import Network.WebSockets.Types
import Network.WebSockets.Protocol.Hybi10 (Hybi10 (..))

data Hybi00 = Hybi00

instance Protocol Hybi00 where
    version       Hybi00 = "hybi00"
    headerVersion Hybi00 = "0"  -- but the client will elide it
    encodeFrame   Hybi00 = encodeFrameHybi00
    decodeFrame   Hybi00 = decodeFrameHybi00
    finishRequest Hybi00 = runErrorT . handshakeHybi00

-- | Protocols compatible with hybi-00
hybi00Protocols :: [SomeProtocol]
hybi00Protocols = [SomeProtocol Hybi10, SomeProtocol Hybi00]

encodeFrameHybi00 :: Encoder Frame
encodeFrameHybi00 _ (Frame True TextFrame pl) =
    BB.fromLazyByteString $ "\0" `BL.append` pl `BL.append` "\255"
-- TODO: prevent the user from doing this using type tags
encodeFrameHybi00 _ _ = error "Not supported"

decodeFrameHybi00 :: Decoder Frame
decodeFrameHybi00 = decodeTextFrame <|> decodeCloseFrame
  where
    decodeTextFrame = do
        _ <- A.word8 $ fromIntegral 0
        utf8string <- A.manyTill A.anyWord8 (A.try . A.word8 . fromIntegral $ 255)
        return $ Frame True TextFrame $ BL.pack utf8string

    decodeCloseFrame = do
        _ <- A.word8 $ fromIntegral 255
        _ <- A.word8 $ fromIntegral 0
        return $ Frame True CloseFrame ""

divBySpaces :: String -> Maybe Int32
divBySpaces str =
  let number = read $ filter isDigit str :: Integer
      spaces = fromIntegral . length $ filter (==' ') str
  in
   if spaces == 0
   then Nothing
   else Just . fromIntegral $ number `div` spaces

handshakeHybi00 :: RequestHttpPart
                   -> ErrorT HandshakeError A.Parser Request
handshakeHybi00 reqHttp@(RequestHttpPart path h) = do
  -- _ <- lift . A.word8 $ fromIntegral 0x0d
  -- _ <- lift . A.word8 $ fromIntegral 0x0a
  case getHeader "Sec-WebSocket-Version" of
    Left _    -> return ()
    Right "0" -> return ()
    Right _   -> throwError NotSupported
  keyPart3 <- lift $ A.take 8
  let numberFromToken =
        maybe (throwError $ MalformedRequest reqHttp
               "Security token does not contain enough spaces")
        (return . encode) .
        divBySpaces . BC.unpack
  keyPart1 <- numberFromToken =<< getHeader "Sec-WebSocket-Key1"
  keyPart2 <- numberFromToken =<< getHeader "Sec-WebSocket-Key2"
  let key = B.concat . BL.toChunks . encode . md5 $
            BL.concat [keyPart1, keyPart2, BL.fromChunks [keyPart3]]
  host <- getHeader "Host"
  -- todo: origin right? (also applies to hybi10)
  origin <- getHeader "Origin"
  let response = Response 101 "WebSocket Protocol Handshake"
                 (("Upgrade", "WebSocket") :
                  ("Connection", "Upgrade") :
                  ("Sec-WebSocket-Location",
                   "ws://" `B.append` host `B.append` path) :
                  ("Sec-WebSocket-Origin", origin) : []) $
                  key
  return $ Request path h response
    where
      getHeader k = case lookup k h of
        Just t  -> return t
        Nothing -> throwError $
                   MalformedRequest reqHttp $ "Header missing: " ++ BC.unpack (CI.original k)
