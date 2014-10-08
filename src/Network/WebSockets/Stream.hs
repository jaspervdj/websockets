--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
{-# LANGUAGE CPP #-}
module Network.WebSockets.Stream
    ( Stream
    , makeStream
    , makeSocketStream
    , makeEchoStream
    , parse
    , write
    , close
    ) where

import           Control.Applicative            ((<$>))
import qualified Control.Concurrent.Chan        as Chan
import           Control.Exception              (throw)
import           Control.Monad                  (forM_)
import qualified Data.Attoparsec.ByteString     as Atto
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import qualified Network.Socket                 as S
import qualified Network.Socket.ByteString      as SB (recv)

#if !defined(mingw32_HOST_OS)
import qualified Network.Socket.ByteString.Lazy as SBL (sendAll)
#else
import qualified Network.Socket.ByteString      as SB (sendAll)
#endif

import           Network.WebSockets.Types


--------------------------------------------------------------------------------
-- | State of the stream
data StreamState
    = Closed !B.ByteString  -- Remainder
    | Open   !B.ByteString  -- Buffer


--------------------------------------------------------------------------------
-- | Lightweight abstraction over an input/output stream.
data Stream = Stream
    { streamIn    :: IO (Maybe B.ByteString)
    , streamOut   :: (Maybe BL.ByteString -> IO ())
    , streamState :: !(IORef StreamState)
    }


--------------------------------------------------------------------------------
makeStream
    :: IO (Maybe B.ByteString)         -- ^ Reading
    -> (Maybe BL.ByteString -> IO ())  -- ^ Writing
    -> IO Stream                       -- ^ Resulting stream
makeStream i o = Stream i o <$> newIORef (Open B.empty)


--------------------------------------------------------------------------------
makeSocketStream :: S.Socket -> IO Stream
makeSocketStream socket = makeStream receive send
  where
    receive = do
        bs <- SB.recv socket 1024
        return $ if B.null bs then Nothing else Just bs

    send Nothing   = return ()
    send (Just bs) =
#if !defined(mingw32_HOST_OS)
        SBL.sendAll socket bs
#else
        forM_ (BL.toChunks bs) (SB.sendAll socket)
#endif


--------------------------------------------------------------------------------
makeEchoStream :: IO Stream
makeEchoStream = do
    chan <- Chan.newChan
    makeStream (Chan.readChan chan) $ \mbBs -> case mbBs of
        Nothing -> Chan.writeChan chan Nothing
        Just bs -> forM_ (BL.toChunks bs) $ \c -> Chan.writeChan chan (Just c)


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
                mbBs <- streamIn stream
                case mbBs of
                    Nothing -> do
                        writeIORef (streamState stream) (Closed B.empty)
                        return Nothing
                    Just bs -> go (Atto.parse parser bs) False
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
            mbBs <- streamIn stream
            case mbBs of
                Nothing -> go (f B.empty) True
                Just bs -> go (f bs) False
    go (Atto.Fail _ _ err) _ = throw (ParseException err)


--------------------------------------------------------------------------------
write :: Stream -> BL.ByteString -> IO ()
write stream = streamOut stream . Just


--------------------------------------------------------------------------------
close :: Stream -> IO ()
close stream = streamOut stream Nothing
