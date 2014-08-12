--------------------------------------------------------------------------------
-- | Demultiplexing of frames into messages
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Network.WebSockets.Hybi13.Demultiplex
    ( FrameType (..)
    , Frame (..)
    , DemultiplexState
    , emptyDemultiplexState
    , demultiplex
    ) where


--------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import           Control.Exception        (Exception, throw)
import           Data.Binary.Get          (runGet, getWord16be)
import qualified Data.ByteString.Lazy     as BL
import           Data.Monoid              (mappend)
import           Data.Typeable            (Typeable)


--------------------------------------------------------------------------------
import           Network.WebSockets.Types


--------------------------------------------------------------------------------
-- | A low-level representation of a WebSocket packet
data Frame = Frame
    { frameFin     :: !Bool
    , frameRsv1    :: !Bool
    , frameRsv2    :: !Bool
    , frameRsv3    :: !Bool
    , frameType    :: !FrameType
    , framePayload :: !BL.ByteString
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | The type of a frame. Not all types are allowed for all protocols.
data FrameType
    = ContinuationFrame
    | TextFrame
    | BinaryFrame
    | CloseFrame
    | PingFrame
    | PongFrame
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | Thrown if the client sends invalid multiplexed data
data DemultiplexException = DemultiplexException
    deriving (Show, Typeable)


--------------------------------------------------------------------------------
instance Exception DemultiplexException


--------------------------------------------------------------------------------
-- | Internal state used by the demultiplexer
newtype DemultiplexState = DemultiplexState
    { unDemultiplexState :: Maybe (FrameType, Builder)
    }


--------------------------------------------------------------------------------
emptyDemultiplexState :: DemultiplexState
emptyDemultiplexState = DemultiplexState Nothing


--------------------------------------------------------------------------------
demultiplex :: DemultiplexState
            -> Frame
            -> (Maybe Message, DemultiplexState)
demultiplex state (Frame fin _ _ _ tp pl) = case tp of
    -- Return control messages immediately, they have no influence on the state
    CloseFrame  -> (Just (ControlMessage (uncurry Close parsedClose)), state)
    PingFrame   -> (Just (ControlMessage (Ping pl)), state)
    PongFrame   -> (Just (ControlMessage (Pong pl)), state)
    -- If we're dealing with a continuation...
    ContinuationFrame -> case unDemultiplexState state of
        -- We received a continuation but we don't have any state. Let's ignore
        -- this fragment...
        Nothing -> (Nothing, DemultiplexState Nothing)
        -- Append the payload to the state
        -- TODO: protect against overflows
        Just (amt, b)
            | not fin   -> (Nothing, DemultiplexState (Just (amt, b')))
            | otherwise -> case amt of
                TextFrame   -> (Just (DataMessage (Text m)), e)
                BinaryFrame -> (Just (DataMessage (Binary m)), e)
                _           -> throw DemultiplexException
          where
            b' = b `mappend` plb
            m = B.toLazyByteString b'
    TextFrame
        | fin       -> (Just (DataMessage (Text pl)), e)
        | otherwise -> (Nothing, DemultiplexState (Just (TextFrame, plb)))
    BinaryFrame
        | fin       -> (Just (DataMessage (Binary pl)), e)
        | otherwise -> (Nothing, DemultiplexState (Just (BinaryFrame, plb)))
  where
    e = emptyDemultiplexState
    plb = B.fromLazyByteString pl
    parsedClose =
        if BL.null pl
            then (1000, "")
            else ((runGet getWord16be) pl, BL.drop 2 pl)
