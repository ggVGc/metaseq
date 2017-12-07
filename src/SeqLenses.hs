module SeqLenses where

import SeqTypes
import Control.Lens
-- import Control.Applicative
import LensExtra
import Data.Sequence (findIndexL)
import Control.Arrow
import Data.IntMap.Strict as M
import qualified Data.Sequence as S
import Data.Maybe


-- TODO: Not actually a lens.. Where should it go?
selectedTrackEntry :: SeqModel -> Maybe (Key, TrackEntry)
selectedTrackEntry m = do
  track <- m^?curTrack
  track^.trackEntries&lookupLE (m^._2.selectedTrackPos)


curArrangement :: Traversal' SeqModel Arrangement
curArrangement f m = (_1.arrangements) (ix (m^._2.currentArrangementIntex) f) m

curTrack :: Traversal' SeqModel ArrTrack
curTrack f m = curArrangement (ix (m^._2.curTrackIndex) f) m


curTrackEntry :: Traversal' SeqModel  TrackEntry
curTrackEntry f m =
  let
    ind = fromMaybe 0 $ do
      (i,_) <- selectedTrackEntry m
      return i
  in
    (curTrack.trackEntries) (ix ind f) m


trigSeqVariation :: Applicative f => SeqIndex -> (TriggerSeq -> f TriggerSeq) -> S.Seq TriggerSeqVariations -> f (S.Seq TriggerSeqVariations )
trigSeqVariation ind =
  (ix (ind^._1).trigGroupSeqs.assoced.index (ind^._2))



trigSeqVariationWithInd :: Applicative f => SeqIndex -> ((Int, TriggerSeq) -> f (Int, TriggerSeq)) -> SeqModel -> f SeqModel
trigSeqVariationWithInd ind f m =
  let
    seqs = m^?_1.triggerSeqGroups.ix (ind^._1).trigGroupSeqs
  in
    seqs&maybe
      (pure m)
      (
        findIndexL(\(i, _) -> i==ind^._2)
        >>> maybe (pure m) (\x ->
            (_1.triggerSeqGroups.ix (ind^._1).trigGroupSeqs.ix x) f m
          )
      )


curSeqId :: Traversal' SeqModel SeqIndex
curSeqId = curTrackEntry.seqId


curTriggerSeq:: Traversal' SeqModel TriggerSeqVariations
curTriggerSeq f model =
  let maybeInd = model^?curSeqId
  in
    maybeInd&maybe
    (pure model)
    (\ind -> (_1.triggerSeqGroups.ix ( ind^._1)) f model)


curTriggerVariation:: Traversal' SeqModel TriggerSeq
curTriggerVariation f model =
  let maybeInd = model^?curSeqId
  in maybeInd&maybe
    (pure model)
    (\ind -> _1 ((triggerSeqGroups.trigSeqVariation ind) f) model)

curTriggerVariationWithInd :: Traversal' SeqModel (Int, TriggerSeq)
curTriggerVariationWithInd f model =
  let maybeInd = model^?curSeqId
  in maybeInd&maybe
    (pure model)
    (\ind -> trigSeqVariationWithInd ind f model)


curLanes :: Traversal' SeqModel (M.IntMap TrigSeqLane)
curLanes =
  curTriggerVariation.trigEntryLanes


curTrigSeqLane :: Traversal' SeqModel TrigSeqLane
curTrigSeqLane f m =
  curLanes (ix (m^._2.curLaneInd) f) m


laneEntryFromLane :: Applicative f => Int -> (TriggerEntry -> f TriggerEntry) -> TrigSeqLane -> f TrigSeqLane
laneEntryFromLane ind f lane =
  let
    len = (getLaneLength lane)
    trigInd = ind `rem` len
    -- rep = ind `div` len
  in
    -- TODO: Handle repetitions here again, when it's re-added to SeqTypes
    -- laneEntries (ix trigInd (ix rep f)) lane
    laneEntries (ix trigInd f) lane

laneEntry :: Applicative f => Int -> (TriggerEntry -> f TriggerEntry) -> SeqModel -> f SeqModel
laneEntry ind=
  curTrigSeqLane . laneEntryFromLane ind


curLaneEntry :: Applicative f => (TriggerEntry -> f TriggerEntry) -> SeqModel -> f SeqModel
curLaneEntry f m =
  (curTrigSeqLane.laneEntryFromLane (m^._2.selectedTriggerEntry)) f m




