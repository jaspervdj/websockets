-- | HTTP utilities
{-# LANGUAGE OverloadedStrings #-}
module Network.WebSockets.Tests.Util.Http
    ( parseResponse
    ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Attoparsec (Parser)
import Data.ByteString.Internal (c2w)
import qualified Data.Attoparsec as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive as CI

import Network.WebSockets.Http

-- | HTTP response parser
parseResponse :: Parser Response
parseResponse = Response
    <$> fmap (read . BC.unpack) code
    <*> message
    <*> A.manyTill header newline
    <*> A.takeByteString
  where
    space = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    code = A.string "HTTP/1.1" *> space *> A.takeWhile1 (/= c2w ' ') <* space
    message = A.takeWhile1 (/= c2w '\r') <* newline
    header = (,)
        <$> (CI.mk <$> A.takeWhile1 (/= c2w ':'))
        <*  A.string ": "
        <*> A.takeWhile1 (/= c2w '\r')
        <*  newline
