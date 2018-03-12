--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Hybi13
    ( headerVersions
    , finishRequest
    , finishResponse
    , encodeMessage
    , encodeMessages
    , decodeMessages
    , createRequest

      -- Internal (used for testing)
    , encodeFrame
    , parseFrame
    ) where


--------------------------------------------------------------------------------
import qualified Data.ByteString.Builder               as B
import           Control.Applicative                   (pure, (<$>))
import           Control.Arrow                         (first)
import           Control.Exception                     (throwIO)
import           Control.Monad                         (forM, liftM, unless,
                                                        when)
import           Data.Binary.Get                       (Get, getInt64be,
                                                        getLazyByteString,
                                                        getWord16be, getWord8)
import           Data.Binary.Put                       (putWord16be, runPut)
import           Data.Bits                             ((.&.), (.|.))
import           Data.ByteString                       (ByteString)
import qualified Data.ByteString.Base64                as B64
import           Data.ByteString.Char8                 ()
import qualified Data.ByteString.Lazy                  as BL
import           Data.Digest.Pure.SHA                  (bytestringDigest, sha1)
import           Data.IORef
import           Data.Monoid                           (mappend, mconcat,
                                                        mempty)
import           Data.Tuple                            (swap)
import           System.Entropy                        as R
import           System.Random                         (RandomGen, newStdGen)


--------------------------------------------------------------------------------
import           Network.WebSockets.Connection.Options
import           Network.WebSockets.Http
import           Network.WebSockets.Hybi13.Demultiplex
import           Network.WebSockets.Hybi13.Mask
import           Network.WebSockets.Stream             (Stream)
import qualified Network.WebSockets.Stream             as Stream
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
headerVersions :: [ByteString]
headerVersions = ["13"]


--------------------------------------------------------------------------------
finishRequest :: RequestHead
              -> Headers
              -> Either HandshakeException Response
finishRequest reqHttp headers = do
    !key <- getRequestHeader reqHttp "Sec-WebSocket-Key"
    let !hash    = hashKey key
        !encoded = B64.encode hash
    return $ response101 (("Sec-WebSocket-Accept", encoded):headers) ""


--------------------------------------------------------------------------------
finishResponse :: RequestHead
               -> ResponseHead
               -> Either HandshakeException Response
finishResponse request response = do
    -- Response message should be one of
    --
    -- - WebSocket Protocol Handshake
    -- - Switching Protocols
    --
    -- But we don't check it for now
    when (responseCode response /= 101) $ Left $
        MalformedResponse response "Wrong response status or message."

    key          <- getRequestHeader  request  "Sec-WebSocket-Key"
    responseHash <- getResponseHeader response "Sec-WebSocket-Accept"
    let challengeHash = B64.encode $ hashKey key
    when (responseHash /= challengeHash) $ Left $
        MalformedResponse response "Challenge and response hashes do not match."

    return $ Response response ""


--------------------------------------------------------------------------------
encodeMessage :: RandomGen g => ConnectionType -> g -> Message -> (g, B.Builder)
encodeMessage conType gen msg = (gen', builder)
  where
    mkFrame      = Frame True False False False
    (mask, gen') = case conType of
        ServerConnection -> (Nothing, gen)
        ClientConnection -> first Just (randomMask gen)
    builder      = encodeFrame mask $ case msg of
        (ControlMessage (Close code pl)) -> mkFrame CloseFrame $
            runPut (putWord16be code) `mappend` pl
        (ControlMessage (Ping pl))               -> mkFrame PingFrame   pl
        (ControlMessage (Pong pl))               -> mkFrame PongFrame   pl
        (DataMessage rsv1 rsv2 rsv3 (Text pl _)) -> Frame True rsv1 rsv2 rsv3 TextFrame   pl
        (DataMessage rsv1 rsv2 rsv3 (Binary pl)) -> Frame True rsv1 rsv2 rsv3 BinaryFrame pl


--------------------------------------------------------------------------------
encodeMessages
    :: ConnectionType
    -> Stream
    -> IO ([Message] -> IO ())
encodeMessages conType stream = do
    genRef <- newIORef =<< newStdGen
    return $ \msgs -> do
        builders <- forM msgs $ \msg ->
          atomicModifyIORef' genRef $ \s -> encodeMessage conType s msg
        Stream.write stream (B.toLazyByteString $ mconcat builders)


--------------------------------------------------------------------------------
encodeFrame :: Maybe Mask -> Frame -> B.Builder
encodeFrame mask f = B.word8 byte0 `mappend`
    B.word8 byte1 `mappend` len `mappend` maskbytes `mappend`
    B.lazyByteString (maskPayload mask payload)
  where

    byte0  = fin .|. rsv1 .|. rsv2 .|. rsv3 .|. opcode
    fin    = if frameFin f  then 0x80 else 0x00
    rsv1   = if frameRsv1 f then 0x40 else 0x00
    rsv2   = if frameRsv2 f then 0x20 else 0x00
    rsv3   = if frameRsv3 f then 0x10 else 0x00
    payload = case frameType f of
        ContinuationFrame -> framePayload f
        TextFrame         -> framePayload f
        BinaryFrame       -> framePayload f
        CloseFrame        -> BL.take 125 $ framePayload f
        PingFrame         -> BL.take 125 $ framePayload f
        PongFrame         -> BL.take 125 $ framePayload f
    opcode = case frameType f of
        ContinuationFrame -> 0x00
        TextFrame         -> 0x01
        BinaryFrame       -> 0x02
        CloseFrame        -> 0x08
        PingFrame         -> 0x09
        PongFrame         -> 0x0a
    (maskflag, maskbytes) = case mask of
        Nothing -> (0x00, mempty)
        Just m  -> (0x80, encodeMask m)

    byte1 = maskflag .|. lenflag
    len'  = BL.length payload
    (lenflag, len)
        | len' < 126     = (fromIntegral len', mempty)
        | len' < 0x10000 = (126, B.word16BE (fromIntegral len'))
        | otherwise      = (127, B.word64BE (fromIntegral len'))


--------------------------------------------------------------------------------
decodeMessages
    :: SizeLimit
    -> SizeLimit
    -> Stream
    -> IO (IO (Maybe Message))
decodeMessages frameLimit messageLimit stream = do
    dmRef <- newIORef emptyDemultiplexState
    return $ go dmRef
  where
    go dmRef = do
        mbFrame <- Stream.parseBin stream (parseFrame frameLimit)
        case mbFrame of
            Nothing    -> return Nothing
            Just frame -> do
                demultiplexResult <- atomicModifyIORef' dmRef $
                    \s -> swap $ demultiplex messageLimit s frame
                case demultiplexResult of
                    DemultiplexError err    -> throwIO err
                    DemultiplexContinue     -> go dmRef
                    DemultiplexSuccess  msg -> return (Just msg)


--------------------------------------------------------------------------------
-- | Parse a frame
parseFrame :: SizeLimit -> Get Frame
parseFrame frameSizeLimit = do
    byte0 <- getWord8
    let fin    = byte0 .&. 0x80 == 0x80
        rsv1   = byte0 .&. 0x40 == 0x40
        rsv2   = byte0 .&. 0x20 == 0x20
        rsv3   = byte0 .&. 0x10 == 0x10
        opcode = byte0 .&. 0x0f

    byte1 <- getWord8
    let mask = byte1 .&. 0x80 == 0x80
        lenflag = byte1 .&. 0x7f

    len <- case lenflag of
        126 -> fromIntegral <$> getWord16be
        127 -> getInt64be
        _   -> return (fromIntegral lenflag)

    -- Check size against limit.
    unless (atMostSizeLimit len frameSizeLimit) $
        fail $ "Frame of size " ++ show len ++ " exceeded limit"

    ft <- case opcode of
        0x00 -> return ContinuationFrame
        0x01 -> return TextFrame
        0x02 -> return BinaryFrame
        0x08 -> enforceControlFrameRestrictions len fin >> return CloseFrame
        0x09 -> enforceControlFrameRestrictions len fin >> return PingFrame
        0x0a -> enforceControlFrameRestrictions len fin >> return PongFrame
        _    -> fail $ "Unknown opcode: " ++ show opcode

    masker <- maskPayload <$> if mask then Just <$> parseMask else pure Nothing

    chunks <- getLazyByteString len

    return $ Frame fin rsv1 rsv2 rsv3 ft (masker chunks)

    where
        enforceControlFrameRestrictions len fin
          | not fin   = fail "Control Frames must not be fragmented!"
          | len > 125 = fail "Control Frames must not carry payload > 125 bytes!"
          | otherwise = pure ()

--------------------------------------------------------------------------------
hashKey :: ByteString -> ByteString
hashKey key = unlazy $ bytestringDigest $ sha1 $ lazy $ key `mappend` guid
  where
    guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    lazy = BL.fromChunks . return
    unlazy = mconcat . BL.toChunks


--------------------------------------------------------------------------------
createRequest :: ByteString
              -> ByteString
              -> Bool
              -> Headers
              -> IO RequestHead
createRequest hostname path secure customHeaders = do
    key <- B64.encode `liftM`  getEntropy 16
    return $ RequestHead path (headers key ++ customHeaders) secure
  where
    headers key =
        [ ("Host"                   , hostname     )
        , ("Connection"             , "Upgrade"    )
        , ("Upgrade"                , "websocket"  )
        , ("Sec-WebSocket-Key"      , key          )
        , ("Sec-WebSocket-Version"  , versionNumber)
        ]

    versionNumber = head headerVersions
