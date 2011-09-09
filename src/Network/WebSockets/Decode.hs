-- | Fast, custom parser for WebSocket data
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Network.WebSockets.Decode
    ( Result (..)
    , Decoder
    , request
    , frame
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Network.WebSockets.Types

-- | Result of a parser
data Result a
    -- | Need more input
    = Incomplete
    -- | Result & remainder
    | Ok !a !ByteString
    -- | Error occurred
    | Error
    deriving (Show)

-- | Alias for parsers
type Decoder a = ByteString -> Result a

-- | Parse an initial request
request :: Decoder Request
request = requestLine . splitLines
  where
    -- Split bytestring into lines
    splitLines bs = case B.breakSubstring "\r\n" bs of
        (b, "") -> [b]
        ("", b) -> "" : B.drop 2 b : []
        (b, br) -> b : splitLines (B.drop 2 br)

    -- Parse the leading request line
    requestLine [] = Incomplete
    requestLine (l : ls) = case BC.split ' ' l of
        ["GET", path, "HTTP/1.1"] -> headerLines path ls
        _                         -> Error
    {-# INLINE requestLine #-}

    -- Parse the header lines
    headerLines p = headerLines' []
      where
        headerLines' _ [] = Incomplete
        headerLines' h (l : ls) = case BC.break (== ':') l of
            ("", "") -> token p h ls
            (_,  "") -> Error
            (k, v)   -> headerLines' ((k, B.drop 2 v) : h) ls
    {-# INLINE headerLines #-}

    -- | Parse the token
    token p h [r]
        | B.length r >= 8 = let (t, r') = B.splitAt 8 r in Ok (Request p h t) r'
        | otherwise = Incomplete
    token _ _ _ = Incomplete
    {-# INLINE token #-}

-- | Parse a frame
frame :: Decoder ByteString
frame bs
    | B.length bs < 2 = Incomplete
    | B.head bs == 0  = case B.break (== 0xff) bs of
        (_, "") -> Incomplete
        (f, br) -> Ok (B.tail f) (B.tail br)
    | otherwise       = Error
