--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Hybi10.Internal
    ( version
    , headerVersions
    , finishRequest
    , finishResponse
    , encodeMessages
    , decodeMessages
    , createRequest
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder              as B
import           Control.Applicative                   (pure, (<$>))
import           Control.Exception                     (throw)
import           Control.Monad                         (liftM)
import           Data.Attoparsec                       (anyWord8)
import qualified Data.Attoparsec                       as A
import           Data.Binary.Get                       (getWord16be,
                                                        getWord64be, runGet)
import           Data.Bits                             ((.&.), (.|.))
import           Data.ByteString                       (ByteString, intercalate)
import qualified Data.ByteString.Base64                as B64
import           Data.ByteString.Char8                 ()
import qualified Data.ByteString.Lazy                  as BL
import           Data.Digest.Pure.SHA                  (bytestringDigest, sha1)
import           Data.Int                              (Int64)
import           Data.IORef
import           Data.Monoid                           (mappend, mconcat,
                                                        mempty)
import           Data.Tuple                            (swap)
import           System.Entropy                        as R
import qualified System.IO.Streams                     as Streams
import qualified System.IO.Streams.Attoparsec          as Streams


--------------------------------------------------------------------------------
import           Network.WebSockets.Handshake.Http
import           Network.WebSockets.Hybi10.Demultiplex
import           Network.WebSockets.Hybi10.Mask
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
version :: String
version = "hybi10"


--------------------------------------------------------------------------------
headerVersions :: [ByteString]
headerVersions = ["13", "8", "7"]


--------------------------------------------------------------------------------
finishRequest :: RequestHead
              -> Response
finishRequest reqHttp =
    let !key     = getRequestHeader reqHttp "Sec-WebSocket-Key"
        !hash    = hashKey key
        !encoded = B64.encode hash
    in response101 [("Sec-WebSocket-Accept", encoded)] ""


--------------------------------------------------------------------------------
finishResponse :: RequestHead
               -> ResponseHead
               -> Response
finishResponse request response
    -- Response message should be one of
    --
    -- - WebSocket Protocol Handshake
    -- - Switching Protocols
    --
    -- But we don't check it for now
    | responseCode response /= 101  = throw $ MalformedResponse response
        "Wrong response status or message."
    | responseHash /= challengeHash = throw $ MalformedResponse response
        "Challenge and response hashes do not match."
    | otherwise                     =
        Response response ""
  where
    key           = getRequestHeader  request  "Sec-WebSocket-Key"
    responseHash  = getResponseHeader response "Sec-WebSocket-Accept"
    challengeHash = B64.encode $ hashKey key


--------------------------------------------------------------------------------
encodeMessage :: Message -> B.Builder
encodeMessage msg = builder
  where
    mkFrame = Frame True False False False
    builder = encodeFrame $ case msg of
        (ControlMessage (Close pl)) -> mkFrame CloseFrame  pl
        (ControlMessage (Ping pl))  -> mkFrame PingFrame   pl
        (ControlMessage (Pong pl))  -> mkFrame PongFrame   pl
        (DataMessage (Text pl))     -> mkFrame TextFrame   pl
        (DataMessage (Binary pl))   -> mkFrame BinaryFrame pl


--------------------------------------------------------------------------------
encodeMessages :: Streams.OutputStream ByteString
               -> IO (Streams.OutputStream Message)
encodeMessages bsStream =
    Streams.builderStream bsStream >>=
    Streams.contramap encodeMessage


--------------------------------------------------------------------------------
encodeFrame :: Frame -> B.Builder
encodeFrame f = B.fromWord8 byte0 `mappend`
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


--------------------------------------------------------------------------------
decodeMessages :: Streams.InputStream ByteString
               -> IO (Streams.InputStream Message)
decodeMessages bsStream = do
    dmRef <- newIORef emptyDemultiplexState
    Streams.makeInputStream $ next dmRef
  where
    next dmRef = do
        frame <- Streams.parseFromStream parseFrame bsStream
        m     <- atomicModifyIORef dmRef $ \s -> swap $ demultiplex s frame
        maybe (next dmRef) (return . Just) m


--------------------------------------------------------------------------------
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
              -> Maybe ByteString
              -> Maybe [ByteString]
              -> Bool
              -> IO RequestHead
createRequest hostname path origin protocols secure = do
    key <- B64.encode `liftM`  getEntropy 16
    return $ RequestHead path (headers key) secure
  where
    headers key = [("Host"                   , hostname     )
                  ,("Connection"             , "Upgrade"    )
                  ,("Upgrade"                , "websocket"  )
                  ,("Sec-WebSocket-Key"      , key          )
                  ,("Sec-WebSocket-Version"  , versionNumber)
                  ] ++ protocolHeader protocols
                    ++ originHeader origin

    originHeader (Just o)    = [("Origin"                , o                  )]
    originHeader Nothing     = []

    protocolHeader (Just ps) = [("Sec-WebSocket-Protocol", intercalate ", " ps)]
    protocolHeader Nothing   = []

    versionNumber = head headerVersions
