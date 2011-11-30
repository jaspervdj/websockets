{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Network.WebSockets.Protocol.Hybi00.Internal
       ( Hybi00_ (..)
       ) where

import Control.Applicative ((<|>))
import Control.Monad.Error (ErrorT (..), MonadError, throwError)
import Control.Monad.Trans (lift)
import Data.Char (isDigit)

import Data.Binary (encode)
import Data.Digest.Pure.MD5 (md5)
import Data.Int (Int32)
import qualified Blaze.ByteString.Builder as BB
import qualified Data.Attoparsec as A
#if MIN_VERSION_attoparsec(0,10,0)
import qualified Data.Attoparsec.Types as AT
#endif
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Protocol
import Network.WebSockets.Types

data Hybi00_ = Hybi00_

instance Protocol Hybi00_ where
    version         Hybi00_ = "hybi00"
    headerVersion   Hybi00_ = "0"  -- but the client will elide it
    encodeFrame     Hybi00_ = encodeFrameHybi00
    decodeFrame     Hybi00_ = decodeFrameHybi00
    finishRequest   Hybi00_ = runErrorT . handshakeHybi00
    implementations         = [Hybi00_]

instance TextProtocol Hybi00_

encodeFrameHybi00 :: Encoder p Frame
encodeFrameHybi00 _ (Frame True TextFrame pl) =
    BB.fromLazyByteString $ "\0" `BL.append` pl `BL.append` "\255"
encodeFrameHybi00 _ (Frame _ CloseFrame _) =
    BB.fromLazyByteString  "\255\0"
    -- TODO: prevent the user from doing this using type tags
encodeFrameHybi00 _ _ = error "Not supported"

decodeFrameHybi00 :: Decoder p Frame
decodeFrameHybi00 = decodeTextFrame <|> decodeCloseFrame
  where
    decodeTextFrame = do
        _ <- A.word8 0x00
        utf8string <- A.manyTill A.anyWord8 (A.try $ A.word8 0xff)
        return $ Frame True TextFrame $ BL.pack utf8string

    decodeCloseFrame = do
        _ <- A.word8 0xff
        _ <- A.word8 0x00
        return $ Frame True CloseFrame ""

divBySpaces :: String -> Maybe Int32
divBySpaces str
    | spaces == 0 = Nothing
    | otherwise   = Just . fromIntegral $ number `div` spaces
  where
    number = read $ filter isDigit str :: Integer
    spaces = fromIntegral . length $ filter (== ' ') str

handshakeHybi00 :: RequestHttpPart
#if MIN_VERSION_attoparsec(0,10,0)
                -> ErrorT HandshakeError (AT.Parser B.ByteString) Request
#else
                -> ErrorT HandshakeError A.Parser Request
#endif
handshakeHybi00 reqHttp@(RequestHttpPart path h) = do
    -- _ <- lift . A.word8 $ fromIntegral 0x0d
    -- _ <- lift . A.word8 $ fromIntegral 0x0a

    case getHeader "Sec-WebSocket-Version" of
        Left _    -> return ()
        Right "0" -> return ()
        Right _   -> throwError NotSupported

    keyPart3 <- lift $ A.take 8
    keyPart1 <- numberFromToken =<< getHeader "Sec-WebSocket-Key1"
    keyPart2 <- numberFromToken =<< getHeader "Sec-WebSocket-Key2"

    let key = B.concat . BL.toChunks . encode . md5 $ BL.concat
                [keyPart1, keyPart2, BL.fromChunks [keyPart3]]

    host <- getHeader "Host"
    -- todo: origin right? (also applies to hybi10)
    origin <- getHeader "Origin"
    let response = response101
            [ ("Sec-WebSocket-Location", B.concat ["ws://", host, path])
            , ("Sec-WebSocket-Origin", origin)
            ]
            key

    return $ Request path h response
  where
    getHeader k = case lookup k h of
        Just t  -> return t
        Nothing -> throwError $ MalformedRequest reqHttp $
            "Header missing: " ++ BC.unpack (CI.original k)

    numberFromToken token = case divBySpaces (BC.unpack token) of
        Just n  -> return $ encode n
        Nothing -> throwError $ MalformedRequest reqHttp
            "Security token does not contain enough spaces"
