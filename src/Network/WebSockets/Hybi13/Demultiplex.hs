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
    | DemultiplexState (Builder -> Message) !Builder

--------------------------------------------------------------------------------
emptyDemultiplexState :: DemultiplexState
emptyDemultiplexState = EmptyDemultiplexState


--------------------------------------------------------------------------------
demultiplex :: DemultiplexState
            -> Frame
            -> (Maybe Message, DemultiplexState)
demultiplex state (Frame True False False False PingFrame pl)
   | BL.length pl > 125 = throw $ CloseRequest 1002 "Protocol Error"
   | otherwise = (Just (ControlMessage (Ping pl)), state)
demultiplex state (Frame True False False False PongFrame pl) = (Just (ControlMessage (Pong pl)), state)
demultiplex _     (Frame True False False False CloseFrame pl)
      = (Just (ControlMessage (uncurry Close parsedClose)), EmptyDemultiplexState)
  where
      -- The Close frame MAY contain a body (the "Application data" portion of the
      -- frame) that indicates a reason for closing, such as an endpoint shutting
      -- down, an endpoint having received a frame too large, or an endpoint
      -- having received a frame that does not conform to the format expected by
      -- the endpoint. If there is a body, the first two bytes of the body MUST
      -- be a 2-byte unsigned integer (in network byte order) representing a
      -- status code with value /code/ defined in Section 7.4.
    parsedClose
       | BL.length pl >= 2 = case runGet getWord16be pl of
              a | a < 1000 -> (1002, BL.empty)
              a -> (a, BL.drop 2 pl)
       | BL.length pl == 1 = (1002, BL.empty)
       | otherwise         = (1000, BL.empty)

demultiplex EmptyDemultiplexState (Frame fin rsv1 rsv2 rsv3 tp pl) = case tp of
    TextFrame
        | fin       -> (Just (DataMessage rsv1 rsv2 rsv3 (Text pl)), e)
        | otherwise -> (Nothing, DemultiplexState (DataMessage rsv1 rsv2 rsv3 . Text . B.toLazyByteString) plb)
    BinaryFrame
        | fin       -> (Just (DataMessage rsv1 rsv2 rsv3 (Binary pl)), e)
        | otherwise -> (Nothing, DemultiplexState (DataMessage rsv1 rsv2 rsv3 . Binary . B.toLazyByteString) plb)
    _ -> throw DemultiplexException
  where
      e   = EmptyDemultiplexState
      plb = B.fromLazyByteString pl

demultiplex (DemultiplexState f b) (Frame fin False False False ContinuationFrame pl)
  | not fin   = (Nothing, DemultiplexState f b')
  | otherwise = (Just (f b'), e)

 where
   b' = b `mappend` plb
   e   = EmptyDemultiplexState
   plb = B.fromLazyByteString pl

demultiplex _ _ = throw DemultiplexException