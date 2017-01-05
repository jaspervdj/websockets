--------------------------------------------------------------------------------
-- | Module dealing with HTTP: request data types, encoding and decoding...
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Network.WebSockets.Http
    ( Headers
    , RequestHead (..)
    , Request (..)
    , ResponseHead (..)
    , Response (..)
    , HandshakeException (..)

    , encodeRequestHead
    , encodeRequest
    , decodeRequestHead

    , encodeResponseHead
    , encodeResponse
    , decodeResponseHead
    , decodeResponse

    , response101
    , response400

    , getRequestHeader
    , getResponseHeader
    , getRequestSecWebSocketVersion
    , getRequestSubprotocols
    , getRequestSecWebSocketExtensions
    ) where


--------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder           as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import           Control.Applicative                (pure, (*>), (<$>), (<*),
                                                     (<*>))
import           Control.Exception.Safe             (Exception)
import qualified Data.Attoparsec.ByteString         as A
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as B
import           Data.ByteString.Char8              ()
import qualified Data.ByteString.Char8              as BC
import           Data.ByteString.Internal           (c2w)
import qualified Data.CaseInsensitive               as CI
import           Data.Dynamic                       (Typeable)
import           Data.Monoid                        (mappend, mconcat)


--------------------------------------------------------------------------------
-- | Request headers
type Headers = [(CI.CI ByteString, ByteString)]


--------------------------------------------------------------------------------
-- | An HTTP request. The request body is not yet read.
data RequestHead = RequestHead
    { requestPath    :: !B.ByteString
    , requestHeaders :: Headers
    , requestSecure  :: Bool
    } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | A request with a body
data Request = Request RequestHead B.ByteString
    deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | HTTP response, without body.
data ResponseHead = ResponseHead
    { responseCode    :: !Int
    , responseMessage :: !B.ByteString
    , responseHeaders :: Headers
    } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- | A response including a body
data Response = Response ResponseHead B.ByteString
    deriving (Show)


--------------------------------------------------------------------------------
-- | Error in case of failed handshake. Will be thrown as an 'Exception'.
--
-- TODO: This should probably be in the Handshake module, and is solely here to
-- prevent a cyclic dependency.
data HandshakeException
    -- | We don't have a match for the protocol requested by the client.
    -- todo: version parameter
    = NotSupported
    -- | The request was somehow invalid (missing headers or wrong security
    -- token)
    | MalformedRequest RequestHead String
    -- | The servers response was somehow invalid (missing headers or wrong
    -- security token)
    | MalformedResponse ResponseHead String
    -- | The request was well-formed, but the library user rejected it.
    -- (e.g. "unknown path")
    | RequestRejected Request String
    -- | for example "EOF came too early" (which is actually a parse error)
    -- or for your own errors. (like "unknown path"?)
    | OtherHandshakeException String
    deriving (Show, Eq, Typeable)


--------------------------------------------------------------------------------
instance Exception HandshakeException


--------------------------------------------------------------------------------
encodeRequestHead :: RequestHead -> Builder.Builder
encodeRequestHead (RequestHead path headers _) =
    Builder.copyByteString "GET "      `mappend`
    Builder.copyByteString path        `mappend`
    Builder.copyByteString " HTTP/1.1" `mappend`
    Builder.fromByteString "\r\n"      `mappend`
    mconcat (map header headers)       `mappend`
    Builder.copyByteString "\r\n"
  where
    header (k, v) = mconcat $ map Builder.copyByteString
        [CI.original k, ": ", v, "\r\n"]


--------------------------------------------------------------------------------
encodeRequest :: Request -> Builder.Builder
encodeRequest (Request head' body) =
    encodeRequestHead head' `mappend` Builder.copyByteString body


--------------------------------------------------------------------------------
-- | Parse an initial request
decodeRequestHead :: Bool -> A.Parser RequestHead
decodeRequestHead isSecure = RequestHead
    <$> requestLine
    <*> A.manyTill decodeHeaderLine newline
    <*> pure isSecure
  where
    space   = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    requestLine = A.string "GET" *> space *> A.takeWhile1 (/= c2w ' ')
        <* space
        <* A.string "HTTP/1.1" <* newline


--------------------------------------------------------------------------------
-- | Encode an HTTP upgrade response
encodeResponseHead :: ResponseHead -> Builder.Builder
encodeResponseHead (ResponseHead code msg headers) =
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


--------------------------------------------------------------------------------
encodeResponse :: Response -> Builder.Builder
encodeResponse (Response head' body) =
    encodeResponseHead head' `mappend` Builder.copyByteString body


--------------------------------------------------------------------------------
-- | An upgrade response
response101 :: Headers -> B.ByteString -> Response
response101 headers = Response
    (ResponseHead 101 "WebSocket Protocol Handshake"
        (("Upgrade", "websocket") : ("Connection", "Upgrade") : headers))


--------------------------------------------------------------------------------
-- | Bad request
response400 :: Headers -> B.ByteString -> Response
response400 headers = Response (ResponseHead 400 "Bad Request" headers)


--------------------------------------------------------------------------------
-- | HTTP response parser
decodeResponseHead :: A.Parser ResponseHead
decodeResponseHead = ResponseHead
    <$> fmap (read . BC.unpack) code
    <*> message
    <*> A.manyTill decodeHeaderLine newline
  where
    space = A.word8 (c2w ' ')
    newline = A.string "\r\n"

    code = A.string "HTTP/1.1" *> space *> A.takeWhile1 (/= c2w ' ') <* space
    message = A.takeWhile1 (/= c2w '\r') <* newline


--------------------------------------------------------------------------------
decodeResponse :: A.Parser Response
decodeResponse = Response <$> decodeResponseHead <*> A.takeByteString


--------------------------------------------------------------------------------
getRequestHeader :: RequestHead
                 -> CI.CI ByteString
                 -> Either HandshakeException ByteString
getRequestHeader rq key = case lookup key (requestHeaders rq) of
    Just t  -> Right t
    Nothing -> Left $ MalformedRequest rq $
        "Header missing: " ++ BC.unpack (CI.original key)


--------------------------------------------------------------------------------
getResponseHeader :: ResponseHead
                  -> CI.CI ByteString
                  -> Either HandshakeException ByteString
getResponseHeader rsp key = case lookup key (responseHeaders rsp) of
    Just t  -> Right t
    Nothing -> Left $ MalformedResponse rsp $
        "Header missing: " ++ BC.unpack (CI.original key)


--------------------------------------------------------------------------------
-- | Get the @Sec-WebSocket-Version@ header
getRequestSecWebSocketVersion :: RequestHead -> Maybe B.ByteString
getRequestSecWebSocketVersion p =
    lookup "Sec-WebSocket-Version" (requestHeaders p)


--------------------------------------------------------------------------------
-- | List of subprotocols specified by the client, in order of preference.
-- If the client did not specify a list of subprotocols, this will be the
-- empty list.
getRequestSubprotocols :: RequestHead -> [B.ByteString]
getRequestSubprotocols rh = maybe [] parse mproto
    where
        mproto = lookup "Sec-WebSocket-Protocol" $ requestHeaders rh
        parse = filter (not . B.null) . BC.splitWith (\o -> o == ',' || o == ' ')

--------------------------------------------------------------------------------
-- | Get the @Sec-WebSocket-Extensions@ header
getRequestSecWebSocketExtensions :: RequestHead -> Maybe B.ByteString
getRequestSecWebSocketExtensions p =
    lookup "Sec-WebSocket-Extensions" (requestHeaders p)

--------------------------------------------------------------------------------
decodeHeaderLine :: A.Parser (CI.CI ByteString, ByteString)
decodeHeaderLine = (,)
    <$> (CI.mk <$> A.takeWhile1 (/= c2w ':'))
    <*  A.word8 (c2w ':')
    <*  A.option (c2w ' ') (A.word8 (c2w ' '))
    <*> A.takeWhile (/= c2w '\r')
    <*  A.string "\r\n"
