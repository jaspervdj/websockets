-- | Fast, custom parser for WebSocket data
{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Network.WebSockets.Parse
    ( Result (..)
    , Parser
    , Request (..)
    , request
    , Frame
    , frame
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

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
type Parser a = ByteString -> Result a

-- | Simple request type
data Request = Request !ByteString [(ByteString, ByteString)] !ByteString
             deriving (Show)

-- | Parse an initial request
request :: Parser Request
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
            ("", "") -> payload p h ls
            (_,  "") -> Error
            (k, v)   -> headerLines' ((k, B.drop 2 v) : h) ls
    {-# INLINE headerLines #-}

    -- | Parse the payload
    payload p h [r]
        | B.length r >= 8 =
            let (pl, r') = B.splitAt 8 r in Ok (Request p h pl) r'
        | otherwise = Incomplete
    payload _ _ _ = Incomplete
    {-# INLINE payload #-}

-- | Alias
type Frame = ByteString

-- | Parse a frame
frame :: Parser ByteString
frame bs
    | B.length bs < 2 = Incomplete
    | B.head bs == 0  = case B.break (== 0xff) bs of
        (_, "") -> Incomplete
        (f, br) -> Ok (B.tail f) (B.tail br)
    | otherwise       = Error
