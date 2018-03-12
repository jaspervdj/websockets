-- | Code for parsing extensions headers.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.WebSockets.Extensions.Description
    ( ExtensionParam
    , ExtensionDescription (..)
    , ExtensionDescriptions

    , parseExtensionDescriptions
    , encodeExtensionDescriptions
    ) where

import           Control.Applicative              ((*>), (<*))
import qualified Data.Attoparsec.ByteString       as A
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import qualified Data.ByteString                  as B
import           Data.Monoid                      (mconcat, mappend)
import           Prelude

type ExtensionParam = (B.ByteString, Maybe B.ByteString)

data ExtensionDescription = ExtensionDescription
    { extName   :: !B.ByteString
    , extParams :: ![ExtensionParam]
    } deriving (Eq, Show)

parseExtensionDescription :: A.Parser ExtensionDescription
parseExtensionDescription = do
    extName   <- parseIdentifier
    extParams <- A.many' (token ';' *> parseParam)
    return ExtensionDescription {..}
  where
    parseIdentifier = AC8.takeWhile isIdentifierChar <* AC8.skipSpace

    token c = AC8.char8 c <* AC8.skipSpace

    isIdentifierChar c =
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        c == '-' || c == '_'

    parseParam :: A.Parser ExtensionParam
    parseParam = do
        name <- parseIdentifier
        val  <- A.option Nothing $ fmap Just $ token '=' *> parseIdentifier
        return (name, val)

encodeExtensionDescription :: ExtensionDescription -> B.ByteString
encodeExtensionDescription ExtensionDescription {..} =
    mconcat (extName : map encodeParam extParams)
  where
    encodeParam (key, Nothing)  = ";" `mappend` key
    encodeParam (key, Just val) = ";" `mappend` key `mappend` "=" `mappend` val

type ExtensionDescriptions = [ExtensionDescription]

parseExtensionDescriptions :: B.ByteString -> Either String ExtensionDescriptions
parseExtensionDescriptions = A.parseOnly $
    AC8.skipSpace *>
    A.sepBy parseExtensionDescription (AC8.char8 ',' <* AC8.skipSpace) <*
    A.endOfInput

encodeExtensionDescriptions :: ExtensionDescriptions -> B.ByteString
encodeExtensionDescriptions = B.intercalate "," . map encodeExtensionDescription
