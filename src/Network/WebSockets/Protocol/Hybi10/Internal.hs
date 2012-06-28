{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Protocol.Hybi10.Internal
    ( Hybi10_ (..)
    , encodeFrameHybi10
    ) where

import Control.Applicative (pure, (<$>))
import Data.Bits ((.&.), (.|.))
import Data.Maybe (maybeToList)
import Data.Monoid (mempty, mappend, mconcat)

import Data.Attoparsec (anyWord8)
import Data.Binary.Get (runGet, getWord16be, getWord64be)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Int (Int64)
import Data.Enumerator ((=$))
import qualified Blaze.ByteString.Builder as B
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Enumerator as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Network.WebSockets.Handshake.Http
import Network.WebSockets.Protocol
import Network.WebSockets.Protocol.Hybi10.Demultiplex
import Network.WebSockets.Protocol.Hybi10.Mask
import Network.WebSockets.Types

data Hybi10_ = Hybi10_

instance Protocol Hybi10_ where
    version         Hybi10_ = "hybi10"
    headerVersions  Hybi10_ = ["13", "8", "7"]
    encodeMessages  Hybi10_ = EL.map encodeMessageHybi10
    decodeMessages  Hybi10_ = decodeMessagesHybi10
    finishRequest   Hybi10_ = handshakeHybi10
    implementations         = [Hybi10_]

instance TextProtocol Hybi10_
instance BinaryProtocol Hybi10_

encodeMessageHybi10 :: Message p -> B.Builder
encodeMessageHybi10 msg = builder
  where
    mkFrame = Frame True False False False
    builder = encodeFrameHybi10 $ case msg of
        (ControlMessage (Close pl)) -> mkFrame CloseFrame  pl
        (ControlMessage (Ping pl))  -> mkFrame PingFrame   pl
        (ControlMessage (Pong pl))  -> mkFrame PongFrame   pl
        (DataMessage (Text pl))     -> mkFrame TextFrame   pl
        (DataMessage (Binary pl))   -> mkFrame BinaryFrame pl

-- | Encode a frame
encodeFrameHybi10 :: Frame -> B.Builder
encodeFrameHybi10 f = B.fromWord8 byte0 `mappend`
    B.fromWord8 byte1 `mappend` len `mappend`
    B.fromLazyByteString (framePayload f)
  where
    byte0  = fin .|. rsv1 .|. rsv2 .|. rsv3 .|. opcode
    fin    = if frameFin f  then 0x80 else 0x00
    rsv1   = if frameRsv1 f then 0x40 else 0x00
    rsv2   = if frameRsv2 f then 0x20 else 0x00
    rsv3   = if frameRsv3 f then 0x10 else 0x00
    opcode = case frameType f of
        ContinuationFrame -> 0x00
        TextFrame         -> 0x01
        BinaryFrame       -> 0x02
        CloseFrame        -> 0x08
        PingFrame         -> 0x09
        PongFrame         -> 0x0a

    byte1 = lenflag
    len'  = BL.length (framePayload f)
    (lenflag, len)
        | len' < 126     = (fromIntegral len', mempty)
        | len' < 0x10000 = (126, B.fromWord16be (fromIntegral len'))
        | otherwise      = (127, B.fromWord64be (fromIntegral len'))

decodeMessagesHybi10 :: Monad m => E.Enumeratee ByteString (Message p) m a
decodeMessagesHybi10 =
    (E.sequence (A.iterParser parseFrame) =$) . demultiplexEnum

demultiplexEnum :: Monad m => E.Enumeratee Frame (Message p) m a
demultiplexEnum = EL.concatMapAccum step emptyDemultiplexState
  where
    step s f = let (m, s') = demultiplex s f in (s', maybeToList m)

-- | Parse a frame
parseFrame :: A.Parser Frame
parseFrame = do
    byte0 <- anyWord8
    let fin    = byte0 .&. 0x80 == 0x80
        rsv1   = byte0 .&. 0x40 == 0x40
        rsv2   = byte0 .&. 0x20 == 0x20
        rsv3   = byte0 .&. 0x10 == 0x10
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

    return $ Frame fin rsv1 rsv2 rsv3 ft (masker $ BL.fromChunks chunks)
  where
    runGet' g = runGet g . BL.fromChunks . return

    take64 :: Int64 -> A.Parser [ByteString]
    take64 n
        | n <= 0    = return []
        | otherwise = do
            let n' = min intMax n
            chunk <- A.take (fromIntegral n')
            (chunk :) <$> take64 (n - n')
      where
        intMax :: Int64
        intMax = fromIntegral (maxBound :: Int)

handshakeHybi10 :: Monad m
                => RequestHttpPart
                -> E.Iteratee ByteString m Request
handshakeHybi10 reqHttp@(RequestHttpPart path h _) = do
    key <- getHeader "Sec-WebSocket-Key"
    let hash = unlazy $ bytestringDigest $ sha1 $ lazy $ key `mappend` guid
    let encoded = B64.encode hash
    return $ Request path h $ response101 [("Sec-WebSocket-Accept", encoded)] ""
  where
    guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    lazy = BL.fromChunks . return
    unlazy = mconcat . BL.toChunks
    getHeader k = case lookup k h of
        Just t  -> return t
        Nothing -> E.throwError $ MalformedRequest reqHttp $ 
            "Header missing: " ++ BC.unpack (CI.original k)
