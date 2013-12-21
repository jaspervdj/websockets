--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Hybi00
    ( headerVersions
    , finishRequest
    ) where


--------------------------------------------------------------------------------
import           Control.Exception        (throw)
import qualified Data.Binary              as Binary
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (isDigit)
import qualified Data.Digest.Pure.MD5     as Md5
import           Data.Int                 (Int32)
import qualified System.IO.Streams        as Streams


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
