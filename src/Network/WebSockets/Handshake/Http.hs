-- | Module dealing with HTTP: request data types, encoding and decoding...
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Network.WebSockets.Handshake.Http
    ( Headers
    , Request (..)
    , RequestHttpPart (..)
    , RequestBody (..)
    , ResponseHttpPart (..)
    , ResponseBody (..)
    , HandshakeError (..)
    , getSecWebSocketVersion

    , encodeRequestHttpPart
    , encodeRequestBody
    , decodeRequest

    , encodeResponseHttpPart
    , encodeResponseBody
    , decodeResponse

    , response101
    , response400

    , getRequestHeader
    , getResponseHeader
    ) where

import Data.Dynamic (Typeable)
import Data.Monoid (mappend, mconcat)
import Control.Applicative (pure, (<$>), (<*>), (*>), (<*))
import Control.Exception (Exception)
import Control.Monad.Error (Error (..))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.ByteString.Internal (c2w)
import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.CaseInsensitive as CI
import qualified Data.Enumerator as E

-- | Request headers
type Headers = [(CI.CI B.ByteString, B.ByteString)]

-- | Full request type, including the response to it
data Request = Request
    { requestPath     :: !B.ByteString
    , requestHeaders  :: Headers
    , requestResponse :: ResponseBody
    } deriving (Show)

-- | (Internally used) HTTP headers and requested path.
data RequestHttpPart = RequestHttpPart
    { requestHttpPath    :: !B.ByteString
    , requestHttpHeaders :: Headers
    , requestHttpSecure  :: Bool
    } deriving (Eq, Show)

-- | A request with a body
data RequestBody = RequestBody RequestHttpPart B.ByteString
    deriving (Show)

-- | Response to a 'Request'
data ResponseHttpPart = ResponseHttpPart
    { responseHttpCode    :: !Int
    , responseHttpMessage :: !B.ByteString
    , responseHttpHeaders :: Headers
    } deriving (Show)

-- | A response including a body
data ResponseBody = ResponseBody ResponseHttpPart B.ByteString
    deriving (Show)

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
    -- | The servers response was somehow invalid (missing headers or wrong
    -- security token)
    | MalformedResponse ResponseHttpPart String
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

-- | RequestHttpPart encoder
encodeRequestHttpPart :: RequestHttpPart -> Builder.Builder
encodeRequestHttpPart (RequestHttpPart path headers _) =
    Builder.copyByteString "GET "      `mappend`
    Builder.copyByteString path        `mappend`
    Builder.copyByteString " HTTP/1.1" `mappend`
    Builder.fromByteString "\r\n"      `mappend`
    mconcat (map header headers)       `mappend`
    Builder.copyByteString "\r\n"
  where
    header (k, v) = mconcat $ map Builder.copyByteString
        [CI.original k, ": ", v, "\r\n"]

-- | RequestBody encoder
encodeRequestBody :: RequestBody -> Builder.Builder
encodeRequestBody (RequestBody httpPart body) =
    encodeRequestHttpPart httpPart `mappend` Builder.copyByteString body

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
        <*> A.takeWhile (/= c2w '\r')
        <*  newline

-- | Encode an HTTP upgrade response
encodeResponseHttpPart :: ResponseHttpPart -> Builder.Builder
encodeResponseHttpPart (ResponseHttpPart code msg headers) =
    Builder.copyByteString "HTTP/1.1 " `mappend`
    Builder.fromString (show code)     `mappend`
    Builder.fromChar ' '               `mappend`
    Builder.fromByteString msg         `mappend`
    Builder.fromByteString "\r\n"      `mappend`
    mconcat (map header headers)       `mappend`
    Builder.copyByteString "\r\n"
  where
    header (k, v) = mconcat $ map Builder.copyByteString
        [CI.original k, ": ", v, "\r\n"]

encodeResponseBody :: ResponseBody -> Builder.Builder
encodeResponseBody (ResponseBody httpPart body) =
    encodeResponseHttpPart httpPart `mappend` Builder.copyByteString body

-- | An upgrade response
response101 :: Headers -> B.ByteString -> ResponseBody
response101 headers = ResponseBody
    (ResponseHttpPart 101 "WebSocket Protocol Handshake"
        (("Upgrade", "websocket") : ("Connection", "Upgrade") : headers))

-- | Bad request
--
response400 :: Headers -> ResponseBody
response400 headers =
    ResponseBody (ResponseHttpPart 400 "Bad Request" headers) ""

-- | HTTP response parser
decodeResponse :: A.Parser ResponseHttpPart
decodeResponse = ResponseHttpPart
    <$> fmap (read . BC.unpack) code
    <*> message
    <*> A.manyTill header newline
  where
    space = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    code = A.string "HTTP/1.1" *> space *> A.takeWhile1 (/= c2w ' ') <* space
    message = A.takeWhile1 (/= c2w '\r') <* newline
    header = (,)
        <$> (CI.mk <$> A.takeWhile1 (/= c2w ':'))
        <*  A.string ": "
        <*> A.takeWhile (/= c2w '\r')
        <*  newline

getRequestHeader :: Monad m
                 => RequestHttpPart
                 -> CI.CI ByteString
                 -> E.Iteratee ByteString m ByteString
getRequestHeader rq key = case lookup key (requestHttpHeaders rq) of
    Just t  -> return t
    Nothing -> E.throwError $ MalformedRequest rq $ 
        "Header missing: " ++ BC.unpack (CI.original key)

getResponseHeader :: Monad m
                  => ResponseHttpPart
                  -> CI.CI ByteString
                  -> E.Iteratee ByteString m ByteString
getResponseHeader rsp key = case lookup key (responseHttpHeaders rsp) of
    Just t  -> return t
    Nothing -> E.throwError $ MalformedResponse rsp $ 
        "Header missing: " ++ BC.unpack (CI.original key)
