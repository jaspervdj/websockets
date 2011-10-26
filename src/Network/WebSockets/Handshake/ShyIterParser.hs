module Network.WebSockets.Handshake.ShyIterParser
    ( shyIterParser
    ) where

import Data.Attoparsec.Enumerator (ParseError(..))
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.Enumerator as E

-- | Behaves almost the same as the 'iterParser' function from the
-- attoparsec-enumerator package, with the exception that, while the iteratee
-- created by 'iterParser' always starts in a 'Continue' state, this one will
-- only ask for more data if the underlying parser needs it. That's important
-- because e.g. network connection will block and wait for more data on a
-- 'Continue'. For messages or frames the problem will not occur as we know the
-- length of the payload and therefore can determine EOF (= end of the frame
-- here).
--
-- We use parsers that don't take data for validation in Protocols.Hybi10
shyIterParser :: (Monad m) => A.Parser a -> E.Iteratee B.ByteString m a
shyIterParser p = parseLoop (A.parse p) [B.empty]
    -- feed empty to the parse *once*.
  where
    -- from here: copied from attoparsec-enumerator-0.2.0.4
    step parse (E.Chunks xs) = parseLoop parse (notEmpty xs)
    step parse E.EOF = case A.feed (parse B.empty) B.empty of
        A.Done _ a -> E.yield a E.EOF
        A.Partial _ -> err [] "iterParser: divergent parser"
        A.Fail _ ctx msg -> err ctx msg

    parseLoop parse [] = E.continue (step parse)
    parseLoop parse (x:xs) = case parse x of
        A.Done extra a -> E.yield a $ if B.null extra
            then E.Chunks xs
            else E.Chunks (extra:xs)
        A.Partial parse' -> parseLoop parse' xs
        A.Fail _ ctx msg -> err ctx msg

    err ctx msg = E.throwError (ParseError ctx msg)
    notEmpty = filter (not . B.null)
