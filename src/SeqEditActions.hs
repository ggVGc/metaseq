module SeqEditActions where
import Control.Lens
import SeqTypes
import SeqLenses
import qualified Data.Vector as V
import Sequencer (addContinuations, findPrevTrigger, findNextTrigger, isContinuationTrigger, deleteContinuations)
import Data.Maybe
import Misc
import Data.IntMap.Strict(keys)
import qualified Data.IntMap.Strict as M

moveCurNotes :: Int -> Int -> SeqModel -> SeqModel
moveCurNotes lineLength step m =
  m&curTrigSeqLane %~ (\lane ->
      lane&moveLaneNotes lineLength step
    )

moveLaneNotes :: Int -> Int -> TrigSeqLane -> TrigSeqLane
moveLaneNotes lineLength step lane =
  lane&laneEntries .~ adjustedLaneEntries
  where
    oldKeys = lane^.laneEntries&keys
    newLaneEntries = moveMapIndices ((lane^.laneLoopLen)-1) step (lane^.laneEntries)
    newKeys = newLaneEntries&keys
    adjustedLaneEntries =
      zip oldKeys newKeys
      \> ffoldl newLaneEntries (\entries (oldK, newK) ->
        let
          deltaLines =  (newK`div`lineLength) - (oldK`div`lineLength)
        in if (abs step)/= lineLength && deltaLines/=0 then
          fromMaybe entries $ do
            v <- M.lookup newK newLaneEntries
            return $
              entries
              \> M.delete newK
              \> M.insert (newK - (lineLength*deltaLines)) v
        else
          entries
      )

setLaneEntry :: Eq a => a -> V.Vector a -> V.Vector a
setLaneEntry defEntry e =
  if e  == mempty then
    V.singleton defEntry
  else
    e


deleteContinuationsFrom :: Int -> Int -> LaneEntries -> LaneEntries
deleteContinuationsFrom fromInd maxLen entries =
  let
    end =
      findNextTrigger (not . isContinuationTrigger) fromInd entries
      & fromMaybe (maxLen - 1)
  in
    entries&deleteContinuations fromInd end


addContinuationsTo :: Int -> LaneEntries -> LaneEntries
addContinuationsTo toInd entries =
  findPrevTrigger (not . isContinuationTrigger) toInd entries
  & maybe entries (\prevTrigInd ->
    entries&addContinuations (prevTrigInd+1) toInd
  )

