-- | Module dealing with HTTP: request data types, encoding and decoding...
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Network.WebSockets.Handshake.Http
    ( Headers
    , RequestHttpPart (..)
    , Request (..)
    , Response (..)
    , HandshakeError (..)
    , getSecWebSocketVersion
    , decodeRequest
    , encodeResponse
    , response101
    , response400
    ) where

import Data.Dynamic (Typeable)
import Data.Monoid (mappend, mconcat)
import Control.Applicative (pure, (<$>), (<*>), (*>), (<*))
import Control.Exception (Exception)
import Control.Monad.Error (Error (..))

import Data.ByteString.Char8 ()
import Data.ByteString.Internal (c2w)
import qualified Data.Attoparsec as A
import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI

-- | Request headers
type Headers = [(CI.CI B.ByteString, B.ByteString)]

-- | (Internally used) HTTP headers and requested path.
data RequestHttpPart = RequestHttpPart
    { requestHttpPath    :: !B.ByteString
    , requestHttpHeaders :: Headers
    , requestHttpSecure  :: Bool
    } deriving (Eq, Show)

-- | Full request type
data Request = Request
    { requestPath     :: !B.ByteString
    , requestHeaders  :: Headers
    , requestResponse :: Response
    }
    deriving (Show)

-- | Response to a 'Request'
data Response = Response
    { responseCode    :: !Int
    , responseMessage :: !B.ByteString
    , responseHeaders :: Headers
    , responseBody    :: B.ByteString
    } deriving (Show)

-- | Error in case of failed handshake. Will be thrown as an iteratee
-- exception. ('Error' condition).
--
-- TODO: This should probably be in the Handshake module, and is solely here to
-- prevent a cyclic dependency.
data HandshakeError
    -- | We don't have a match for the protocol requested by the client.
    -- todo: version parameter
    = NotSupported
    -- | The request was somehow invalid (missing headers or wrong security
    -- token)
    | MalformedRequest RequestHttpPart String
    -- | The request was well-formed, but the library user rejected it.
    -- (e.g. "unknown path")
    | RequestRejected Request String
    -- | for example "EOF came too early" (which is actually a parse error)
    -- or for your own errors. (like "unknown path"?)
    | OtherHandshakeError String
    deriving (Show, Typeable)

instance Error HandshakeError where
    strMsg = OtherHandshakeError

instance Exception HandshakeError

-- | Get the @Sec-WebSocket-Version@ header
getSecWebSocketVersion :: RequestHttpPart -> Maybe B.ByteString
getSecWebSocketVersion p = lookup "Sec-WebSocket-Version" (requestHttpHeaders p)

-- | Parse an initial request
decodeRequest :: Bool -> A.Parser RequestHttpPart
decodeRequest isSecure = RequestHttpPart
    <$> requestLine
    <*> A.manyTill header newline
    <*> pure isSecure
  where
    space   = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    requestLine = A.string "GET" *> space *> A.takeWhile1 (/= c2w ' ')
        <* space
        <* A.string "HTTP/1.1" <* newline

    header = (,)
        <$> (CI.mk <$> A.takeWhile1 (/= c2w ':'))
        <*  A.string ": "
        <*> A.takeWhile1 (/= c2w '\r')
        <*  newline

-- | Encode an HTTP upgrade response
encodeResponse :: Response -> Builder.Builder
encodeResponse (Response code msg headers body) =
    Builder.copyByteString "HTTP/1.1 " `mappend`
    Builder.fromString (show code)     `mappend`
    Builder.fromChar ' '               `mappend`
    Builder.fromByteString msg         `mappend`
    Builder.fromByteString "\r\n"      `mappend`
    mconcat (map header headers)       `mappend`
    Builder.copyByteString "\r\n"      `mappend`
    Builder.copyByteString body  -- (body is empty except for version -00)
  where
    header (k, v) = mconcat $ map Builder.copyByteString
        [CI.original k, ": ", v, "\r\n"]

-- | An upgrade response
response101 :: Headers -> B.ByteString -> Response
response101 headers body = Response 101 "WebSocket Protocol Handshake"
    (("Upgrade", "websocket") : ("Connection", "Upgrade") : headers)
    body

-- | Bad request
--
response400 :: Headers -> Response
response400 headers = Response 400 "Bad Request" headers ""
