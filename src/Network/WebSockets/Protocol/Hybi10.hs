{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Protocol.Hybi10
    ( hybi10
    ) where

import Control.Applicative (pure, (<$>))
import Data.Bits ((.&.), (.|.))
import Data.Monoid (mappend, mempty)

import Data.Attoparsec (anyWord8)
import Data.Binary.Get (runGet, getWord16be, getWord64be)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Int (Int64)
import qualified Blaze.ByteString.Builder as B
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Control.Monad.Error (Error (..), throwError)
import Data.Monoid (mappend, mconcat)

import Network.WebSockets.Decode (Decoder)
import Network.WebSockets.Encode (Encoder)
import Network.WebSockets.Mask
-- import Network.WebSockets.Protocol (Protocol (..))
import Network.WebSockets.Types
import qualified Network.WebSockets.Feature as F

import Control.Monad


-- | Parse a frame
decodeFrameHybi10 :: Decoder Frame
decodeFrameHybi10 = do
    byte0 <- anyWord8
    let fin = byte0 .&. 0x80 == 0x80
        opcode = byte0 .&. 0x0f

    let ft = case opcode of
            0x00 -> ContinuationFrame
            0x01 -> TextFrame
            0x02 -> BinaryFrame
            0x08 -> CloseFrame
            0x09 -> PingFrame
            0x0a -> PongFrame
            _    -> error "Unknown opcode"

    byte1 <- anyWord8
    let mask = byte1 .&. 0x80 == 0x80
        lenflag = fromIntegral (byte1 .&. 0x7f)

    len <- case lenflag of
        126 -> fromIntegral . runGet' getWord16be <$> A.take 2
        127 -> fromIntegral . runGet' getWord64be <$> A.take 8
        _   -> return lenflag

    masker <- maskPayload <$> if mask then Just <$> A.take 4 else pure Nothing

    chunks <- take64 len

    return $ Frame fin ft (masker $ BL.fromChunks chunks)
  where
    runGet' g = runGet g . BL.fromChunks . return

    take64 :: Int64 -> Decoder [ByteString]
    take64 n
        | n <= 0    = return []
        | otherwise = do
            let n' = min intMax n
            chunk <- A.take (fromIntegral n')
            (chunk :) <$> take64 (n - n')
      where
        intMax :: Int64
        intMax = fromIntegral (maxBound :: Int)

-- | Encode a frame
encodeFrameHybi10 :: Encoder Frame
encodeFrameHybi10 mask f = B.fromWord8 byte0 `mappend`
    B.fromWord8 byte1 `mappend` len `mappend` maskbytes `mappend`
    B.fromLazyByteString (maskPayload mask (framePayload f))
  where
    byte0  = fin .|. opcode
    fin    = if frameFin f then 0x80 else 0x00
    opcode = case frameType f of
        ContinuationFrame -> 0x00
        TextFrame         -> 0x01
        BinaryFrame       -> 0x02
        CloseFrame        -> 0x08
        PingFrame         -> 0x09
        PongFrame         -> 0x0a

    (maskflag, maskbytes) = case mask of
        Nothing -> (0x00, mempty)
        Just m  -> (0x80, B.fromByteString m)

    byte1 = maskflag .|. lenflag
    len'  = BL.length (framePayload f)
    (lenflag, len)
        | len' < 126     = (fromIntegral len', mempty)
        | len' < 0x10000 = (126, B.fromWord16be (fromIntegral len'))
        | otherwise      = (127, B.fromWord64be (fromIntegral len'))


handshakeHybi10 :: RequestHttpPart -> Decoder (Either HandshakeError Request)
handshakeHybi10 reqHttp@(RequestHttpPart path h) = return $ do
    case getHeader "Sec-WebSocket-Version" of
        Right "8" -> return ()
        _         -> throwError NotSupported
    key <- getHeader "Sec-WebSocket-Key"
    let hash = unlazy $ bytestringDigest $ sha1 $ lazy $ key `mappend` guid
    let encoded = B64.encode hash
    return $ Request path h
      $ response101 [("Sec-WebSocket-Accept", encoded)]
  where
    guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    lazy = BL.fromChunks . return
    unlazy = mconcat . BL.toChunks
    getHeader k = case lookup k h of
        Just t  -> return t
        Nothing -> throwError $
                   MalformedRequest reqHttp $ 
                   "Header missing: " ++ BC.unpack (CI.original k)

-- | An upgrade response
response101 :: Headers -> Response
response101 headers = Response 101 "WebSocket Protocol Handshake" 
    (("Upgrade", "WebSocket") :
    ("Connection", "Upgrade") :
    headers)
    ""

-- | Bad request
response400 :: Headers -> Response
response400 headers = Response 400 "Bad Request" headers ""

featuresHybi10 :: F.Features
featuresHybi10 = F.unions [F.binary, F.ping, F.fragmentation, F.close]

hybi10 :: Protocol
hybi10 = Protocol
    { version = "hybi10"
    , headerVersion = "8"
    , encodeFrame = encodeFrameHybi10
    , decodeFrame = decodeFrameHybi10
    , finishRequest = handshakeHybi10
    , features = featuresHybi10
    }

