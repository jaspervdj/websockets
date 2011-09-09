-- | Implementation of the WebSocket handshake
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Handshake
    ( HandshakeError (..)
    , handshake
    ) where

import Data.Monoid (mappend)
import Data.Char (isDigit)
import Data.Int (Int32)
import Control.Monad.Error (Error (..), throwError)

import Data.Binary (encode)
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import Network.WebSockets.Types

-- | Error in case of failed handshake.
data HandshakeError = HandshakeError String
                    deriving (Show)

instance Error HandshakeError where
    noMsg = HandshakeError "Handshake error"
    strMsg = HandshakeError

handshake :: Request -> Either HandshakeError Response
handshake (Request path headers token) = do
    origin <- getHeader "Origin"
    key1 <- getHeader "Sec-WebSocket-Key1"
    key2 <- getHeader "Sec-WebSocket-Key2"
    host <- getHeader "Host"

    let token' = createToken key1 key2 token
    let location = "ws://" `mappend` host `mappend` path

    return $ Response
        [ ("Upgrade", "WebSocket")
        , ("Connection", "Upgrade")
        , ("Sec-WebSocket-Origin", origin)
        , ("Sec-WebSocket-Location", location)
        , ("Sec-WebSocket-Protocol", "sample")
        ] token'
  where
    getHeader k = case lookup k headers of
        Just t  -> return t
        Nothing -> throwError $
            HandshakeError $ "Header missing: " ++ BC.unpack k

createToken :: ByteString -> ByteString -> ByteString -> ByteString
createToken key1 key2 token = B.pack $ BL.unpack (encode hash)
  where
    hash         = md5 $ BL.concat [num1, num2, token']
    [num1, num2] = map (encode . divBySpaces) [key1, key2]
    token'       = BL.fromChunks [token]

divBySpaces :: ByteString -> Int32
divBySpaces str
    | spaces == 0 = 0
    | otherwise   = fromIntegral $ number `div` spaces
  where
    number = read $ BC.unpack (BC.filter isDigit str) :: Integer
    spaces = fromIntegral . B.length $ BC.filter (== ' ') str
