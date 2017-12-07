{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module SeqTypes where

import Control.Lens
import SeqConstants
import AppModeName
import Input
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as M
import Data.Binary
import GHC.Generics (Generic)
import Data.Vector.Binary()
import Misc(ffoldl)


data ControllerState = ControllerState {
  _rootSeqEditPage :: Int
  ,_selectedTriggerEntry :: Int
  ,_rootCenterNote :: Int
  ,_centerEditNote :: NoteEntry
  ,_curTrackIndex :: Int
  ,_curLaneInd :: Int
  ,_lastSelTrigger :: TriggerData
  ,_aMode :: AppModeName
  ,_conInp :: InputState
  ,_selectedTrackPos :: Int
  ,_arrViewOffset :: (Int, Int)
  ,_arrViewOffset2 :: (Int, Int)
  ,_laneViewOffset :: (Int, Int)
  ,_laneViewCompact :: Bool
  ,_arrViewNotesActive :: Bool
  ,_arrView2active :: Bool
  ,_newLaneLoopLen :: Int

  ,_seqEditMoverActive :: Bool
  ,_seqEditShowAll :: Bool
  ,_seqEditViewOffset :: (Int,Int)

  ,_laneMoverMoveAll :: Bool

  ,_currentArrangementIntex :: Int
  ,_arrModeStepsPerEntry :: Int
} deriving (Eq, Read, Show)


type Arrangement = S.Seq ArrTrack

data SharedModel = SharedModel {
  _triggerSeqGroups :: S.Seq TriggerSeqVariations
      -- List of trigger sequences.
  ,_noteMapping ::  [[Int]]
      -- List of global note mappings.
  ,_arrangements::  S.Seq Arrangement
  ,_noteMapIndex :: Int
  ,_isRecording :: Bool
  ,_bpm :: Int
} deriving (Eq, Read, Show, Generic)


instance Binary SharedModel


-- instance Binary SharedModel

type SeqModel = (SharedModel, ControllerState)


data ArrTrack = ArrTrack {
  _trackEntries :: M.IntMap TrackEntry
  ,_channel :: Int
  ,_followGlobalNoteMap :: Bool
} deriving (Eq, Read, Show, Generic)

instance Binary ArrTrack


type RootNoteMap = M.IntMap Int

data TrackEntry = TrackEntry {
  _seqId :: SeqIndex
      -- Index into triggerSeqGroups
  ,_teLoopCount :: Int
  ,_rootNoteSeq :: RootNoteMap
  ,_teLength :: Int
} deriving(Eq, Read, Show, Generic)

instance Binary TrackEntry

data TriggerSeqVariations = TriggerSeqVariations {
  _trigGroupSeqs :: S.Seq (Int, TriggerSeq) -- variations of sequence
} deriving(Eq, Read, Show, Generic)

instance Binary TriggerSeqVariations


data TriggerSeq = TriggerSeq {
  _trigEntryLanes :: M.IntMap TrigSeqLane
  -- ,_seqFullLength :: Int
  -- ,_loopCount :: Int
} deriving(Eq, Read, Show, Generic)

instance Binary TriggerSeq




type LaneEntries = M.IntMap TriggerEntry

data TrigSeqLane = TrigSeqLane {
  _laneEntries :: LaneEntries
  ,_laneLoopLen :: Int
  -- ,_laneLockedNote :: NoteEntry
} deriving (Eq, Read, Show, Generic)


instance Binary TrigSeqLane


data TriggerData = TriggerData {
  _vel :: Int
  ,_triggerDouble :: Bool
  ,_delay :: Int
} deriving (Eq, Read, Show, Generic)
instance Binary TriggerData


data TriggerEntry =
  TriggerEntry TriggerData
  | ContinuationEntry
  deriving(Eq, Read, Show, Generic)

instance Binary TriggerEntry


data NoteEntry = NoteEntry {
  _nNum :: Int
  ,_octave :: Int
} deriving (Eq, Show, Read, Generic)

instance Binary NoteEntry


type SeqIndex = (Int, Int) -- (Sequence index, Variation index)


velStepFromVal :: Integral b => Int -> b
velStepFromVal v =
  let
    step = fromIntegral (v - minVelocity)
    top = fromIntegral (maxVelocity - minVelocity)
    frac = step / top
  in
    floor (frac*(fromIntegral maxVelStep))


defaultNoteEntry = NoteEntry 0 4


defaultTriggerData :: TriggerData
defaultTriggerData = TriggerData (velForStep 6) False 0


defaultTriggerSeq :: TriggerSeq
defaultTriggerSeq = TriggerSeq
  (M.fromList ([0..127]&map(\i -> (i, defaultTrigSeqLane sequenceDefaultLen))))


defaultTriggerSeqGroup :: TriggerSeqVariations
defaultTriggerSeqGroup = TriggerSeqVariations (S.fromList [(0, defaultTriggerSeq)])


defaultTrackEntry :: TrackEntry
defaultTrackEntry = TrackEntry (0,0) 1 mempty sequenceDefaultLen


defaultArrTrack :: Bool -> ArrTrack
defaultArrTrack = ArrTrack M.empty 0


defaultTrigSeqLane :: Int -> TrigSeqLane
defaultTrigSeqLane = TrigSeqLane mempty



makePrisms ''TriggerEntry
makeLenses ''TriggerData
makeLenses ''NoteEntry
makeLenses ''ArrTrack
makeLenses ''TrackEntry
makeLenses ''TriggerSeqVariations
makeLenses ''TriggerSeq
makeLenses ''TrigSeqLane
makeLenses ''SharedModel
makeLenses ''ControllerState


getLaneLength :: TrigSeqLane -> Int
getLaneLength lane =
  lane^.laneLoopLen


getSeqLength :: TriggerSeq -> Int
getSeqLength trigSeq =
  trigSeq^.trigEntryLanes
  & ffoldl sequenceDefaultLen (\longest lane ->
    let
      l = getLaneLength lane
    in
      if l > longest then l
      else longest
  )

