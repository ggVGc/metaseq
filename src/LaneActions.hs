{-# LANGUAGE LambdaCase #-}
module LaneActions where

import SeqTypes
import Data.Maybe
import Control.Lens
import SeqLenses
import qualified Data.IntMap.Strict as M
import Misc
import Control.Monad.State.Lazy
import Control.Arrow
import Data.List(elemIndex, find)
import Sequencer
import SeqEditActions

setActiveLane :: Int -> SeqModel -> (SharedModel, ControllerState)
setActiveLane laneInd m =
  m
  & editLane laneInd id
  & _2.curLaneInd .~ laneInd



editLane :: M.Key -> (TrigSeqLane -> TrigSeqLane) -> SeqModel -> SeqModel
editLane laneInd f m =
  m&curLanes %~ (
      adjustMapWithDefault (defaultTrigSeqLane (m^._2.newLaneLoopLen)) laneInd f
    )




editCurLane :: (TrigSeqLane -> TrigSeqLane) -> SeqModel -> SeqModel
editCurLane f m =
  m&editLane (m^._2.curLaneInd) f


copyLaneNotes :: Bool -> Int -> Int -> SeqModel -> SeqModel
copyLaneNotes deleteOriginals fromLaneInd toLaneInd m = fromMaybe m $ do
  sq <- m^?curTriggerVariation
  fromLane <- sq^?trigEntryLanes.ix fromLaneInd

  let newLane toLane =
        toLane
        -- & laneEntries %~ M.unionWith (<>) (fromLane^.laneEntries)
        -- TODO: Combine repetition data here, when implemented again in SeqTypes
        & laneEntries %~ M.union  (fromLane^.laneEntries)
        & laneLoopLen .~ fromLane^.laneLoopLen

  return $
      m
      & editLane toLaneInd newLane
      & (if deleteOriginals then editLane fromLaneInd (const $ defaultTrigSeqLane (fromLane^.laneLoopLen)) else id)



-- expandLane :: Bool -> TrigSeqLane -> M.Map Int TriggerEntry
-- expandLane withDoubles lane =
expandLane :: Int -> TrigSeqLane -> M.IntMap TriggerEntry
expandLane maxLen lane =
  execState expand M.empty
  where
    laneLen = getLaneLength lane
    reps =  maxLen `div` laneLen
    ents = lane^.laneEntries
    expand =
      [(rep,trig) | rep<-[0..reps], trig<- [0..(laneLen-1)]]
      & mapM(\(repInd, trigInd)->
          (M.lookup trigInd ents)&fmap (\trig ->
              -- TODO: index into repetition map here, when implemented in SeqTypes
                state $ \m ->
                  let newM = M.insert (repInd*laneLen + trigInd) trig m
                  in (newM, newM)
            )
            & fromMaybe get
        )




getActiveLaneNotes' :: (Enum t, Num t) =>
  Int -> TrigSeqLane -> Int -> SeqModel -> Maybe [(TriggerData, t)]
getActiveLaneNotes' offset lane len m =
        zip (sparseListFrom' len $ expandLane len lane) [0..]
        \> drop offset
        \> map (\(mbTrig,ind) ->
          mbTrig >>=(\case
            ContinuationEntry -> Nothing
            TriggerEntry x ->
              Just $ (x, ind)
          )
        )
        \> catMaybes
        \> (\case
            []-> Nothing
            x->Just x
          )


getActiveLaneNotes :: (Enum t, Num t) =>
  Int -> SeqModel -> Maybe [(TriggerData, t)]
getActiveLaneNotes laneInd m =
  seqMaybe >>= (\sq ->
    sq^?trigEntryLanes.ix laneInd >>= (\lane -> getActiveLaneNotes' 0 lane (sq&getSeqLength) m))
  where
    seqMaybe= m^?curTriggerVariation



getCurActiveLaneNotes :: (Enum t, Num t) =>
    SeqModel -> Maybe [(TriggerData, t)]
getCurActiveLaneNotes m =
  m&getActiveLaneNotes (m^._2.curLaneInd)


-- TODO: Completely ridiculous, fix some day
-- Returns ((trigger, triggerIndex), laneIndex, laneScreenIndex)
getAllLaneActiveNotes ::  Int -> Int -> SeqModel -> Maybe [[((TriggerData, Int), Int, Int)]]
getAllLaneActiveNotes offsetX offsetY m =
  activeLanes&fmap(\(len, lanes) ->
    let processed = processLanes M.empty len 0 lanes
    in
      processed
      & M.elems
      & drop (countOccupiedUntilOffset processed)
      & map(filter(\(_,_,actInd)->actInd >= 0))
  )
  where
    countOccupiedUntilOffset mp =
      mp
      & M.keys
      & takeWhile(\x -> x < offsetX)
      & length


    seqMaybe= m^?curTriggerVariation
    activeLanes = seqMaybe >>= (\sq ->
        sq^?trigEntryLanes&fmap(\lanes ->
          lanes
          & M.assocs
          & mapMaybe(\(laneInd, lane)->
              (getActiveLaneNotes' 0 lane (sq&getSeqLength) m)
              & fmap(map $ \l -> (l,laneInd))
            )
          & (\actives -> (sq&getSeqLength, actives))
        )
      )

    processLanes mp maxLen ind lanes =
      let
        ents =
          lanes
          & mapInd (,)
          & mapMaybe(\(entries, activeInd) -> case entries of
              ((a,i),li):_ -> if i == ind then Just ((a,i),li, activeInd-offsetY) else Nothing
              _ -> Nothing
            )

        newMap = case ents of
          [] -> mp
          xs -> mp&adjustMapWithDefault [] ind (++ xs)

        newLanes = lanes&map(\case
            ((a,i), li):rest -> if i == ind then rest else ((a,i),li):rest
            xs -> xs
          )

      in
        if ind == maxLen-1 then
          mp
        else
          processLanes newMap maxLen (ind+1) newLanes



editNoteOffset :: SeqModel -> Int
editNoteOffset m =
  m
  \> getCurActiveLaneNotes >>= (map snd >>> elemIndex (m^._2.selectedTriggerEntry))
  \> fromMaybe 0
  \> max 0



prevSelectedNote :: SeqModel -> Maybe Int
prevSelectedNote m =
  m
  \> getCurActiveLaneNotes >>= (
      reverse
      >>>map snd
      >>>find(\ind -> ind < (m^._2.selectedTriggerEntry))
    )


nextSelectedNote :: SeqModel -> Maybe Int
nextSelectedNote m =
  m
  \> getCurActiveLaneNotes >>= (
    map snd
    >>> find (\ind -> ind > m^._2.selectedTriggerEntry)
  )


setLaneEntryLengths :: Int -> TrigSeqLane -> TrigSeqLane
setLaneEntryLengths len lane =
  lane&laneEntries %~ (\origEntries ->
    let
      maxLen = lane^.laneLoopLen
      inds =
        origEntries
        & M.filter (not . isContinuationTrigger)
        & M.keys
    in
      inds&ffoldl origEntries (\accumEnts i ->
        deleteContinuationsFrom i maxLen accumEnts
        & addContinuationsTo (i+len)
      ))

