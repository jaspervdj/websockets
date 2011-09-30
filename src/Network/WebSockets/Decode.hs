-- | Provides parsers for the WebSocket protocol. Uses the attoparsec library.
{-# LANGUAGE BangPatterns, OverloadedStrings, PatternGuards #-}
module Network.WebSockets.Decode
    ( Decoder
    , request
    ) where

import Control.Applicative ((<$>), (<*>), (*>), (<*))

import Data.Attoparsec (Parser, string, takeWhile1, word8)
import Data.Attoparsec.Combinator (manyTill)
import Data.ByteString.Char8 ()
import Data.ByteString.Internal (c2w)
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Types

-- | An alias so we don't have to import attoparsec everywhere
type Decoder a = Parser a

-- | Parse an initial request
request :: Decoder Request
request = Request
    <$> requestLine
    <*> manyTill header newline
  where
    space = word8 (c2w ' ')
    newline = string "\r\n"

    requestLine = string "GET" *> space *> takeWhile1 (/= c2w ' ')
        <* space
        <* string "HTTP/1.1" <* newline

    header = (,)
        <$> (CI.mk <$> takeWhile1 (/= c2w ':'))
        <*  string ": "
        <*> takeWhile1 (/= c2w '\r')
        <*  newline
