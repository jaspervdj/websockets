--------------------------------------------------------------------------------
-- | Demultiplexing of frames into messages
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Network.WebSockets.Hybi13.Demultiplex
    ( FrameType (..)
    , Frame (..)
    , DemultiplexState
    , emptyDemultiplexState
    , DemultiplexResult (..)
    , demultiplex
    ) where


--------------------------------------------------------------------------------
import           Data.ByteString.Builder               (Builder)
import qualified Data.ByteString.Builder               as B
import           Control.Exception                     (Exception)
import           Data.Binary.Get                       (getWord16be, runGet)
import qualified Data.ByteString.Lazy                  as BL
import           Data.Int                              (Int64)
import           Data.Monoid                           (mappend)
import           Data.Typeable                         (Typeable)
import           Network.WebSockets.Connection.Options
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
    | DemultiplexState !Int64 !Builder !(Builder -> Message)


--------------------------------------------------------------------------------
emptyDemultiplexState :: DemultiplexState
emptyDemultiplexState = EmptyDemultiplexState


--------------------------------------------------------------------------------
-- | Result of demultiplexing
data DemultiplexResult
    = DemultiplexSuccess  Message
    | DemultiplexError    ConnectionException
    | DemultiplexContinue


--------------------------------------------------------------------------------
demultiplex :: SizeLimit
            -> DemultiplexState
            -> Frame
            -> (DemultiplexResult, DemultiplexState)

demultiplex _ state (Frame True False False False PingFrame pl)
    | BL.length pl > 125 =
        (DemultiplexError $ CloseRequest 1002 "Protocol Error", emptyDemultiplexState)
    | otherwise =
        (DemultiplexSuccess $ ControlMessage (Ping pl), state)

demultiplex _ state (Frame True False False False PongFrame pl) =
    (DemultiplexSuccess (ControlMessage (Pong pl)), state)

demultiplex _ _ (Frame True False False False CloseFrame pl) =
    (DemultiplexSuccess (ControlMessage (uncurry Close parsedClose)), emptyDemultiplexState)
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
              a | a < 1000 || a `elem` [1004,1005,1006
                                       ,1014,1015,1016
                                       ,1100,2000,2999
                                       ,5000,65535] -> (1002, BL.empty)
              a -> (a, BL.drop 2 pl)
       | BL.length pl == 1 = (1002, BL.empty)
       | otherwise         = (1000, BL.empty)

demultiplex sizeLimit EmptyDemultiplexState (Frame fin rsv1 rsv2 rsv3 tp pl) = case tp of
    _ | not (atMostSizeLimit size sizeLimit) ->
        ( DemultiplexError $ ParseException $
            "Message of size " ++ show size ++ " exceeded limit"
        , emptyDemultiplexState
        )

    TextFrame
        | fin       ->
            (DemultiplexSuccess (text pl), emptyDemultiplexState)
        | otherwise ->
            (DemultiplexContinue, DemultiplexState size plb (text . B.toLazyByteString))


    BinaryFrame
        | fin       -> (DemultiplexSuccess (binary pl), emptyDemultiplexState)
        | otherwise -> (DemultiplexContinue, DemultiplexState size plb (binary . B.toLazyByteString))

    _ -> (DemultiplexError $ CloseRequest 1002 "Protocol Error", emptyDemultiplexState)

  where
    size     = BL.length pl
    plb      = B.lazyByteString pl
    text   x = DataMessage rsv1 rsv2 rsv3 (Text x Nothing)
    binary x = DataMessage rsv1 rsv2 rsv3 (Binary x)

demultiplex sizeLimit (DemultiplexState size0 b f) (Frame fin False False False ContinuationFrame pl)
    | not (atMostSizeLimit size1 sizeLimit) =
        ( DemultiplexError $ ParseException $
            "Message of size " ++ show size1 ++ " exceeded limit"
        , emptyDemultiplexState
        )
    | fin         = (DemultiplexSuccess (f b'), emptyDemultiplexState)
    | otherwise   = (DemultiplexContinue, DemultiplexState size1 b' f)
  where
    size1 = size0 + BL.length pl
    b'    = b `mappend` plb
    plb   = B.lazyByteString pl

demultiplex _ _ _ =
    (DemultiplexError (CloseRequest 1002 "Protocol Error"), emptyDemultiplexState)
