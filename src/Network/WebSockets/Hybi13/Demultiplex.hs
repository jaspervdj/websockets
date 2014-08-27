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
data DemultiplexState
    = EmptyDemultiplexState
    | DemultiplexState !FrameType !Builder


--------------------------------------------------------------------------------
emptyDemultiplexState :: DemultiplexState
emptyDemultiplexState = EmptyDemultiplexState


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
    ContinuationFrame -> case state of
        -- We received a continuation but we don't have any state. Let's ignore
        -- this fragment...
        EmptyDemultiplexState -> (Nothing, EmptyDemultiplexState)
        -- Append the payload to the state
        -- TODO: protect against overflows
        DemultiplexState amt b
            | not fin   -> (Nothing, DemultiplexState amt b')
            | otherwise -> case amt of
                TextFrame   -> (Just (DataMessage (Text m)), e)
                BinaryFrame -> (Just (DataMessage (Binary m)), e)
                _           -> throw DemultiplexException
          where
            b' = b `mappend` plb
            m = B.toLazyByteString b'
    TextFrame
        | fin       -> (Just (DataMessage (Text pl)), e)
        | otherwise -> (Nothing, DemultiplexState TextFrame plb)
    BinaryFrame
        | fin       -> (Just (DataMessage (Binary pl)), e)
        | otherwise -> (Nothing, DemultiplexState BinaryFrame plb)
  where
    e   = emptyDemultiplexState
    plb = B.fromLazyByteString pl

    -- The Close frame MAY contain a body (the "Application data" portion of the
    -- frame) that indicates a reason for closing, such as an endpoint shutting
    -- down, an endpoint having received a frame too large, or an endpoint
    -- having received a frame that does not conform to the format expected by
    -- the endpoint. If there is a body, the first two bytes of the body MUST
    -- be a 2-byte unsigned integer (in network byte order) representing a
    -- status code with value /code/ defined in Section 7.4.
    parsedClose
        | BL.length pl >= 2 = (runGet getWord16be pl, BL.drop 2 pl)
        | otherwise         = (1000, "")
