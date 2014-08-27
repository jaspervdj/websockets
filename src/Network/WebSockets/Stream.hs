--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
module Network.WebSockets.Stream
    ( Stream
    , makeStream
    , makeSocketStream
    , parse
    , write
    ) where

import           Control.Applicative            ((<$>))
import           Control.Exception              (throw)
import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import qualified Network.Socket                 as S
import qualified Network.Socket.ByteString      as SB (recv)
import qualified Network.Socket.ByteString.Lazy as SBL (sendAll)

import           Network.WebSockets.Types


--------------------------------------------------------------------------------
-- | State of the stream
data StreamState
    = Closed !B.ByteString  -- Remainder
    | Open   !B.ByteString  -- Buffer


--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
data Stream = Stream
    { streamIn    :: IO B.ByteString  -- Empty ByteString indicates EOF
    , streamOut   :: BL.ByteString -> IO ()
    , streamState :: !(IORef StreamState)
    }


--------------------------------------------------------------------------------
makeStream
    :: IO B.ByteString           -- ^ Reading, empty ByteString indicates EOF.
    -> (BL.ByteString -> IO ())  -- ^ Writing some chunks.
    -> IO Stream                 -- ^ Resulting stream
makeStream i o = Stream i o <$> newIORef (Open B.empty)


--------------------------------------------------------------------------------
makeSocketStream :: S.Socket -> IO Stream
makeSocketStream socket =
    makeStream (SB.recv socket 1024) (SBL.sendAll socket)


--------------------------------------------------------------------------------
parse :: Stream -> Atto.Parser a -> IO (Maybe a)
parse stream parser = do
    state <- readIORef (streamState stream)
    case state of
        Closed remainder
            | B.null remainder -> return Nothing
            | otherwise        -> go (Atto.parse parser remainder) True
        Open buffer
            | B.null buffer -> do
                byteString <- streamIn stream
                go (Atto.parse parser byteString) (B.null byteString)
            | otherwise     -> go (Atto.parse parser buffer) False
  where
    -- Buffer is empty when entering this function.
    go (Atto.Done remainder x) closed = do
        writeIORef (streamState stream) $
            if closed then Closed remainder else Open remainder
        return (Just x)
    go (Atto.Partial f) closed
        | closed    = go (f B.empty) True
        | otherwise = do
            byteString <- streamIn stream
            go (f byteString) (B.null byteString)
    go (Atto.Fail _ _ err) _ = throw (ParseException err)


--------------------------------------------------------------------------------
write :: Stream -> BL.ByteString -> IO ()
write = streamOut
