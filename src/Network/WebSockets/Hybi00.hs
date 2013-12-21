--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Hybi00
    ( headerVersions
    , finishRequest
    , encodeMessages
    , decodeMessages
    ) where


--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder     (Builder)
import qualified Blaze.ByteString.Builder     as Builder
import           Control.Applicative          ((<$>), (<|>))
import           Control.Exception            (throw)
import qualified Data.Attoparsec              as A
import qualified Data.Binary                  as Binary
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy         as BL
import           Data.Char                    (isDigit)
import qualified Data.Digest.Pure.MD5         as Md5
import           Data.Int                     (Int32)
import           Data.Monoid                  (mappend)
import qualified System.IO.Streams            as Streams
import qualified System.IO.Streams.Attoparsec as Streams


--------------------------------------------------------------------------------
import           Network.WebSockets.Http
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
headerVersions :: [ByteString]
headerVersions = []


--------------------------------------------------------------------------------
finishRequest :: RequestHead -> Streams.InputStream ByteString -> IO Response
finishRequest reqHead is = do
    keyPart3 <- Streams.readExactly 8 is
    keyPart1 <- numberFromToken $ getHeader "Sec-WebSocket-Key1"
    keyPart2 <- numberFromToken $ getHeader "Sec-WebSocket-Key2"

    let key    = B.concat . BL.toChunks . Binary.encode . Md5.md5 $ BL.concat
                    [keyPart1, keyPart2, BL.fromChunks [keyPart3]]
        host   = getHeader "Host"
        origin = getHeader "Origin"
        schema = if requestSecure reqHead then "wss://" else "ws://"

    return $ response101
        [ ("Sec-WebSocket-Location", B.concat [schema, host, path])
        , ("Sec-WebSocket-Origin", origin)
        ]
        key
  where
    path      = requestPath reqHead
    getHeader = getRequestHeader reqHead
    numberFromToken token = case divBySpaces (BC.unpack token) of
        Just n  -> return $ Binary.encode n
        Nothing -> throw $ MalformedRequest reqHead
            "Security token does not contain enough spaces"


--------------------------------------------------------------------------------
divBySpaces :: String -> Maybe Int32
divBySpaces str
    | spaces == 0 = Nothing
    | otherwise   = Just . fromIntegral $ number `div` spaces
  where
    number = read $ filter isDigit str :: Integer
    spaces = fromIntegral . length $ filter (== ' ') str


--------------------------------------------------------------------------------
encodeMessage :: Message -> Builder
encodeMessage (DataMessage (Text pl)) =
    (Builder.fromLazyByteString $ "\0" `mappend` pl `mappend` "\255") `mappend`
    Builder.flush
encodeMessage (ControlMessage (Close _)) =
    Builder.fromLazyByteString "\255\0" `mappend` Builder.flush
encodeMessage msg = error $
    "Network.WebSockets.Hybi00.encodeMessage: unsupported message: " ++
    show msg


--------------------------------------------------------------------------------
encodeMessages :: Streams.OutputStream Builder
               -> IO (Streams.OutputStream Message)
encodeMessages bStream =
    Streams.lockingOutputStream =<< Streams.contramap encodeMessage bStream


--------------------------------------------------------------------------------
decodeMessages :: Streams.InputStream ByteString
               -> IO (Streams.InputStream Message)
decodeMessages bsStream = Streams.makeInputStream $
    Just <$> Streams.parseFromStream parseMessage bsStream


--------------------------------------------------------------------------------
parseMessage :: A.Parser Message
parseMessage = parseText <|> parseClose
  where
    parseText = do
        _ <- A.word8 0x00
        utf8string <- A.manyTill A.anyWord8 (A.try $ A.word8 0xff)
        return $ DataMessage $ Text $ BL.pack utf8string

    parseClose = do
        _ <- A.word8 0xff
        _ <- A.word8 0x00
        return $ ControlMessage $ Close ""
