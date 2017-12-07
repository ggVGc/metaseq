{-# LANGUAGE LambdaCase #-}
module Sequencer(
  module Sequencer
  ,module SeqTypes
--  ,module SeqConstants
  ,module SeqLenses
)where

import Misc
import Data.Maybe
-- import Data.List
import Control.Arrow
import Control.Lens
import Data.List (find)
import SeqTypes
import SeqConstants
import SeqLenses
import AppModeName
import ControllerFunctions
import Input
import Prelude hiding (lookup)
import qualified Data.Sequence as S
import qualified Data.IntMap.Strict as M
import Data.Foldable(toList)
import LensExtra
import Data.Sequence (findIndexL)
import ControllerCommon
-- import Misc

{-
   import qualified Data.List.NonEmpty as NE
   import Data.List.NonEmpty (NonEmpty((:|)))
-}


-- majorScale rootNote =
--   map ((+) rootNote) [ 0,2,4,5,7,9,11 ]


initTriggerEntries :: S.Seq TriggerSeqVariations
initTriggerEntries = (S.replicate defaultSeqCount defaultTriggerSeqGroup)


noteToNum' :: Int -> NoteEntry -> Int
noteToNum' notesInOctave note =
  notesInOctave*(note^.octave) + (note^.nNum)


noteToNum :: NoteEntry -> SharedModel -> Int
noteToNum note model =
  noteToNum' (model&curNoteMapping&length) note


trackNoteToNum :: NoteEntry -> SeqModel -> Int
trackNoteToNum note model =
  noteToNum' (model&curTrackNoteMapping&length) note


curNoteMapping :: SharedModel -> [Int]
curNoteMapping model =
  model^?noteMapping.ix(model^.noteMapIndex)
  \> fromMaybe (error "Notemap index out of range")


curTrackNoteMapping :: SeqModel -> [Int]
curTrackNoteMapping model =
  model^?curTrack
  \> fmap (\track ->
    if track^.followGlobalNoteMap then nm
    else identityNoteMap
  )
  \> fromMaybe nm
  where
    nm = model^._1&curNoteMapping


defaultArrangements :: S.Seq Arrangement
defaultArrangements =
  ((replicate ( trackCount `div` 2) (defaultArrTrack True))
    ++(replicate ( trackCount `div` 2) (defaultArrTrack False))
  )
  \> mapInd (\t ind ->
    t
    \> channel .~ ind
  )
  \> S.fromList
  \> S.singleton


defaultCenterNote :: Int
defaultCenterNote = 0


seqEditSelectIsActive :: Int -> ControllerState -> Bool
seqEditSelectIsActive ind m =
  ind /= m^.selectedTriggerEntry


defaultControllerState :: ControllerState
defaultControllerState = ControllerState {
  _rootSeqEditPage = 0
  ,_selectedTriggerEntry = 0
  ,_rootCenterNote = defaultCenterNote
  ,_centerEditNote = defaultNoteEntry
  ,_curTrackIndex = 0
  ,_curLaneInd = baseDrumNote
  ,_lastSelTrigger = defaultTriggerData
  ,_aMode = ArrangeMode
  ,_conInp = defaultInputState
  ,_selectedTrackPos = 0
  ,_arrViewOffset = (0,0)
  ,_arrViewOffset2 = (0,0)
  ,_laneViewOffset = (0,0)
  ,_laneViewCompact = True
  ,_arrViewNotesActive = False
  ,_arrView2active = False
  ,_newLaneLoopLen = defaultLoopLen

  ,_seqEditMoverActive = False
  ,_seqEditShowAll = False
  ,_seqEditViewOffset = (0,0)

  ,_laneMoverMoveAll = False

  ,_currentArrangementIntex = 0
  ,_arrModeStepsPerEntry = sequenceDefaultLen `div` 4
}


defaultSharedModel :: SharedModel
defaultSharedModel = SharedModel
  initTriggerEntries
  defaultNoteMap
  defaultArrangements
  0
  False
  110


curTrigSeqLaneAndVariation :: SeqModel -> Maybe (TrigSeqLane, TriggerSeqVariations)
curTrigSeqLaneAndVariation m =
  m^?curTrigSeqLane >>= (\lane ->
    m^?curTriggerSeq&fmap(\s ->
      (lane, s)
    )
  )


curSeqAndLane :: SeqModel -> Maybe (TriggerSeq, TrigSeqLane)
curSeqAndLane model =
  model^?curTriggerVariation >>= (\sq ->
    sq^?trigEntryLanes.ix(model^._2.curLaneInd)&fmap (\lane ->
      (sq, lane)
    )
  )



numToNote' :: Int -> Int -> NoteEntry
numToNote' notesInOctave n = NoteEntry{
  _nNum = n `mod` notesInOctave
  ,_octave = n `div` notesInOctave
}


numToNote :: Int -> SharedModel -> NoteEntry
numToNote note model=
  numToNote' (model&curNoteMapping&length) note


trackNumToNote :: Int -> SeqModel -> NoteEntry
trackNumToNote note model=
  numToNote' (model&curTrackNoteMapping&length) note


getRealNote :: Int -> NoteEntry -> NoteEntry -> NoteEntry
getRealNote notesInOctave rootNote offset =
  NoteEntry{
    _octave = ( rootNote^.octave ) + ( offset^.octave ) - ( defaultNoteEntry^.octave )
    ,_nNum = ( rootNote^.nNum ) + ( offset^.nNum )
  }
  \> (noteToNum' notesInOctave >>> numToNote' notesInOctave)


mapNote :: [Int] -> NoteEntry -> NoteEntry -> Int
mapNote translations rootNote inNote =
  (note^.octave)*12 + translateNote (note^.nNum)
  where
    note = getRealNote (translations&length) rootNote inNote
    translateNote noteNum = translations & (
        drop noteNum
        >>> take 1
        >>> listToMaybe
        >>> fromMaybe (error $ "BUG: Note value somehow out of range(should never happen) "++show note)
      )


mapTriggerNote :: t -> NoteEntry -> SeqModel -> Int
mapTriggerNote _ {- trigIndMb -} inNote m =
  mapNote noteMap root inNote
  where
    (root, noteMap) =
      m^?curTrack
      \> maybe (defaultNoteEntry, m^._1&curNoteMapping) (\t ->
        if t^.followGlobalNoteMap then
          (defaultNoteEntry, m^._1&curNoteMapping)
        else
          (NoteEntry 0 0, identityNoteMap)
      )
    -- TODO: Map to global root notes
    -- rootNotes = ((m^.rootNoteSeqs)^?ix (track^.rootSeqID))
    -- root = trigIndMb&maybe defaultNoteEntry (\i -> rootNotes^?ix i)


setSeqForCurTrackEntry :: Int -> SeqModel -> SeqModel
setSeqForCurTrackEntry ind model =
  model&curTrack %~ (\track ->
    let replaceInd = model^._2.selectedTrackPos
    in if replaceInd < length (track^.trackEntries) then
        track & (trackEntries.ix replaceInd %~ (seqId%~(_1.~ind).(_2.~0)))
      else
        track
  )


isVariationsEmpty :: TriggerSeqVariations -> Bool
isVariationsEmpty vars =
  (vars^?trigGroupSeqs.assoced.index 0)
  \> maybe
    (False `debug` "Defaulted")
    (\x ->
      x^.trigEntryLanes&all (\lane ->
            lane^.laneEntries == mempty
          )
    )


firstEmptySeq :: Int -> SharedModel -> Maybe TrackEntry
firstEmptySeq trackInd m =
  (m^?triggerSeqGroups.ix (traceThis trackInd))
  >>= (\curVariations ->
    if isVariationsEmpty curVariations then Just trackInd
    else Nothing
  )
  \> flip maybe Just (
    (m^.triggerSeqGroups)
    \> toList
    \> (mapInd (,))
    \> find(\(v, _) -> isVariationsEmpty v)
    \> fmap snd
  )
  \> fmap (\x -> defaultTrackEntry&seqId .~ (x, 0))


hasSameSeq :: Maybe TrackEntry -> Maybe TrackEntry ->  Bool
hasSameSeq entry1 entry2 = fromMaybe False $ do
  e1 <- entry1
  e2 <- entry2
  return $ (e1^.seqId ) == (e2^.seqId)


setToSeqVariation :: Int -> SeqModel -> SeqModel
setToSeqVariation num m =
  m&(curTrackEntry.seqId._2 .~ (
    (m&curTrigGroupSeqs)>>=(\trigs->trigs^?(ix num)._1)&fromMaybe 0
  ))


startNewSeqVariation :: SeqModel -> SeqModel
startNewSeqVariation m = fromMaybe m $ do
  seqInd <- m^?curTrackEntry.seqId._1
  return $
    m
    \> (_1.triggerSeqGroups .ix seqInd.trigGroupSeqs %~ (\s ->
        (S.<|) (length s, fromJust $ m^?curTriggerVariation) s
      ))
    \> setToSeqVariation 0


olderSeqVariation :: SeqModel -> SeqModel
olderSeqVariation m =
  m&stepSeqVariation (\ind len -> min ( len-1) (ind+1))


newerSeqVariation :: SeqModel -> SeqModel
newerSeqVariation m =
  m&stepSeqVariation (\ind _ -> max 0 (ind-1))


curTrigGroupSeqs :: SeqModel -> Maybe (S.Seq (Int, TriggerSeq))
curTrigGroupSeqs m =
    m^?curTrackEntry.seqId._1
    >>= \seqInd ->
      m^?_1.triggerSeqGroups.ix seqInd.trigGroupSeqs

stepSeqVariation :: (Int -> Int -> Int) -> SeqModel -> SeqModel
stepSeqVariation f model =
  model
  \> curTrigGroupSeqs
  \> maybe model (flip stepVariation model)
  where
    stepVariation trigs = curTrackEntry.seqId._2 %~ (modEntry trigs)

    modEntry trigs key =
      trigs^?ix newInd._1
      \> fromMaybe(error"stepSeqVariation: BUG")
      where
        newInd = f ind (length trigs)
        ind = trigs
          \> findIndexL(\(k, _)-> k == key)
          \> fromMaybe (error "stepSeqVariation: BUG")


pushVariationCommon :: ((Int, TriggerSeq) -> S.Seq (Int, TriggerSeq) -> S.Seq (Int, TriggerSeq)) -> SeqModel -> SeqModel
pushVariationCommon moveFunc m =
  m^?curTriggerVariationWithInd&maybe m (\curSeq ->
      m&_1.triggerSeqGroups.ix seqInd.trigGroupSeqs %~ (\s ->
          moveFunc curSeq s
        )
    )
  where
    seqInd = fromJust $ m^?curTrackEntry.seqId._1


bringSeqVariationToFront :: SeqModel -> SeqModel
bringSeqVariationToFront =
  pushVariationCommon smoveItemToHead


pushSeqVariationToBack :: SeqModel -> SeqModel
pushSeqVariationToBack m =
  m
  \> pushVariationCommon smoveItemToBack
  \> setToSeqVariation 0


cycleId :: Int -> SharedModel -> Int -> Int
cycleId step m =
  boundedAdd 0 ((m^.triggerSeqGroups&length) - 1) step


curNewNote :: SeqModel -> (NoteEntry, Int)
curNewNote model =
  let trig = model^._2.lastSelTrigger
  in (numToNote' 12 (model^._2.curLaneInd), trig^.vel)


indexTrigSeqs :: SeqIndex -> S.Seq TriggerSeqVariations -> Maybe (TriggerSeq, TriggerSeqVariations)
indexTrigSeqs i list =
  list^?ix (i^._1) >>=(\subSeqs ->
      (subSeqs^?trigGroupSeqs.assoced.index (i^._2)&fmap(\s->(s, subSeqs)))
    )



updateLastTrigEntry :: SeqModel -> SeqModel
updateLastTrigEntry m =
    m&_2.lastSelTrigger %~ (\t ->
      m^?curLaneEntry
      & fmap (\case
        ContinuationEntry -> t
        TriggerEntry x -> x
      )
      & fromMaybe t
    )


longestTrack :: [ArrTrack] -> Maybe (Int, ArrTrack)
longestTrack tracks =
  tracks\> ffoldl Nothing (\old t ->
      let
        oldLen = old\>maybe 0 fst
        len = t^.trackEntries&highestMapIndex
      in
        if len > oldLen then
          Just $ (len, t)
        else old
    )


curSelectedCell :: ControllerFunctions -> SeqModel -> Cell
curSelectedCell funcs m =
  (funcs&gridFromIndex) (m^._2.selectedTriggerEntry)


curLaneNote :: SeqModel -> NoteEntry
curLaneNote m =
  m&trackNumToNote (m^._2.curLaneInd)


defaultNewEntry :: SeqModel -> TriggerEntry
defaultNewEntry model =
  let
    (n, v) = model&curNewNote
  in
    TriggerEntry $ defaultTriggerData&vel .~ v


findNextTrigger :: (TriggerEntry -> Bool) -> Int -> LaneEntries -> Maybe Int
findNextTrigger predicate startInd entries =
  entries
  & M.toAscList
  & find (\(k, v) ->
      k > startInd && (predicate v)
    )
  & fmap fst


findPrevTrigger :: (TriggerEntry -> Bool) -> Int -> LaneEntries -> Maybe Int
findPrevTrigger predicate startInd entries =
  entries
  & M.toDescList
  & find (\(k, v) ->
      k <= startInd && (predicate v)
    )
  & fmap fst


deleteContinuations :: Int -> Int -> LaneEntries -> LaneEntries
deleteContinuations fromIndex toIndex =
  modMapRange fromIndex toIndex processEntries
  where
    processEntries mbOldVal = mbOldVal >>= \case
      ContinuationEntry -> Nothing
      TriggerEntry x -> Just $ TriggerEntry x


addContinuations :: Int -> Int -> LaneEntries -> LaneEntries
addContinuations fromIndex toIndex =
  modMapRange fromIndex toIndex processEntries
  where
    processEntries = const $
      Just ContinuationEntry


isContinuationTrigger :: TriggerEntry -> Bool
isContinuationTrigger = \case
  ContinuationEntry -> True
  _ -> False


editLane' :: M.Key -> (TrigSeqLane -> TrigSeqLane) -> M.IntMap TrigSeqLane -> M.IntMap TrigSeqLane
editLane' =
  adjustMapWithDefault (defaultTrigSeqLane 1)


setTrigSeqEntry :: Int -> Int -> TriggerEntry -> TriggerSeq  -> TriggerSeq
setTrigSeqEntry laneInd ind entry trSeq =
  trSeq&trigEntryLanes %~(
    editLane' laneInd (laneEntries %~ M.insert ind entry)
  )


