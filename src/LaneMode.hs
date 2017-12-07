{-# LANGUAGE LambdaCase #-}

module LaneMode where

import AppMode
import Input
import SeqTypes
import Control.Lens
import Data.Maybe
import ControllerFunctions
import ControllerCommon
import SeqQueries
import Misc
import SeqLenses
import Sequencer
import Control.Arrow
import Data.List
import SeqEditMode(seqEditMode)
import LaneActions
import Data.Monoid((<>))
import qualified Data.IntMap.Strict as M
import SeqConstants (trigLaneCount)
import Player
import SeqConstants(defaultVelocity)

laneMode :: AppMode
laneMode = runModes [
    runModeIfDown [RowPos$Top 7] seqEditMode
  ] (
      contOptionToggler (RowPos(Side 2)) laneMoverMoveAll
      <>noteMover
      <>laneModeCore
    )



noteMover = nopMode{
  _update = \_ inp m _ ->
    let
      selLane = m^._2.curLaneInd
      mbCurTrig = m^?curLaneEntry
      (selTrig, loopPoint) = fromMaybe (0,0) $ do
        let trigInd = m^._2.selectedTriggerEntry
        lane <- m^?curTrigSeqLane
        return $ (trigInd `mod` (lane^.laneLoopLen), lane^.laneLoopLen)

      moveNote newLane =
        m
        & (mbCurTrig&maybe id (\curTrig ->
            editLane newLane (
              (laneLoopLen .~ loopPoint)
              . (laneEntries %~ M.insert selTrig curTrig)
            )
          ))
        & curLanes.ix selLane.laneEntries %~ M.delete selTrig

      newLaneInd step = boundedAdd 0 trigLaneCount step selLane

      doMove step =
        (if (m^._2.laneMoverMoveAll) then
          m&copyLaneNotes True selLane (newLaneInd step)
        else
          moveNote (newLaneInd step)
        )
        & _2.curLaneInd .~ (newLaneInd step)

      newModel = case inp^.ev of
        InDown(RowPos(Side 0))-> Just $ doMove  (-1)
        InDown(RowPos(Side 1)) -> Just $ doMove  1
        _ -> Nothing

      actions = fromMaybe [] $ do
        laneInd <- case inp^.ev of
            InDown(RowPos(Side 0))-> Just $ newLaneInd (-1)
            InDown(RowPos(Side 1)) -> Just $ newLaneInd 1
            _ -> Nothing
        return [PlayNoteOneShot laneInd defaultVelocity]
    in
      newModel
      & fmap (\newm -> ((), UpdateSingle, newm, actions))
}



laneModeCore = AppMode LaneMode.renderPlayPos  LaneMode.update LaneMode.render


update :: AppModeUpdate
update _ input m funcs =
  mbModFun&fmap(\f -> ((), UpdateSingle, f m, []))
  where
    mbModFun =
      case input^.ev of
        InUp (RowPos(Top 7)) ->
            Just $
              _2.laneViewOffset._1 .~ (m^._2.selectedTriggerEntry)
              >>> (\x ->
                      [0..3]&ffoldl x (\mm _ ->
                        (mm::SeqModel)&_2.laneViewOffset._1 %~ (prev mm)
                      )
                    )
        InDown cell -> case cell of
          RowPos(Side 7) ->
            Just $
              _2.laneViewCompact %~ not
              >>> _2.laneViewOffset._1 .~ m^._2.selectedTriggerEntry
          RowPos(Top 0) ->
            Just $ _2.laneViewOffset._1 %~ prev m
          RowPos(Top 1) ->
            Just $ _2.laneViewOffset._1 %~ next m
          RowPos(Top 2) ->
            Just $ _2.laneViewOffset._2 +~ 1
          RowPos(Top 3) ->
            Just $ _2.laneViewOffset._2 %~ (\x -> max 0 (x - 1))
          GridXY x y ->
            Just $ onGridPress funcs (input&keyDown Shift) (input^.isDoubleTap) x y
          _ -> Nothing
        _ -> Nothing

    compact = m^._2.laneViewCompact

    prev model x =
      if compact then
        prevActiveNoteInd model&fromMaybe x
      else
        max 0 (x - 1)

    next model x =
      if compact then
        nextActiveNoteInd model&fromMaybe x
      else
        x+1



nextActiveNoteInd m =
  m&getAllLaneActiveNotes offX offY >>=(
      drop 1
      >>> listToMaybe
    )>>=(
      listToMaybe
      >>> fmap(\((_,i),_,_)->i)
    )
  where
    offX = m^._2.laneViewOffset._1
    offY = m^._2.laneViewOffset._2

prevActiveNoteInd m =
  m&getAllLaneActiveNotes 0 offY
  >>= (
    takeWhile (\x -> x&listToMaybe&maybe False  (\((_,i),_,_)->  i<=offX-1))
    >>> reverse
    >>> listToMaybe
  )
  >>=(
    listToMaybe
    >>> fmap(\((_,i),_,_)->i)
  )
  where
    offX = m^._2.laneViewOffset._1
    offY = m^._2.laneViewOffset._2

select :: ControllerFunctions -> Int -> Int -> SeqModel -> SeqModel
select funcs x y m =
  m
  & _2.curLaneInd %~ (\i ->
      m&getOccupiedLaneIndFromY funcs y
      & fromMaybe i
    )
  & _2.selectedTriggerEntry .~ x
  & _2.seqEditViewOffset._2 .~ x `div` (funcs&viewWidth)


-- TODO: This is a complete mess, together with getAllLaneActiveNotes and compactRender
getLaneAndEntryFromXY :: ControllerFunctions -> Int -> Int -> SeqModel -> Maybe (Int, Int)
getLaneAndEntryFromXY funcs x y m =
  (m&getAllLaneActiveNotes (m^._2.laneViewOffset._1) (m^._2.laneViewOffset._2)) >>=
    (drop x
    >>> listToMaybe) >>= (
        expand
        >>> drop y
        >>> listToMaybe
        )>>= fmap(\((_,trigInd),laneInd, _)->
          (laneInd, trigInd)
    )
  where
    expand ents =
      [0..(funcs&viewHeight)]&map(\i ->
        ents&find(\((_,_),_,laneScreenInd) -> laneScreenInd == i)
      )

compactSelect :: ControllerFunctions -> Int -> Int -> SeqModel -> SeqModel
compactSelect funcs x y m =
  m&getLaneAndEntryFromXY funcs x y
  & maybe m (\(laneInd, trigInd) ->
      m
      & setActiveLane laneInd
      & _2.selectedTriggerEntry .~ trigInd
      & _2.seqEditViewOffset._2 .~ (funcs&viewHeight)*((trigInd `div` (funcs&viewWidth)) `div` (funcs&viewHeight))
    )

-- editCompactNote :: ControllerFunctions -> Bool -> Int -> Int -> () -> SeqModel -> SeqModel
editCompactNote funcs shiftHeld x y f m = fromMaybe m $ do
  let
    allLanes = fromMaybe [] $ getAllLaneActiveNotes 0 offY m
    allLanesFromHere = fromMaybe [] $ getAllLaneActiveNotes offX offY m
  lanes <-
    allLanesFromHere
    & drop x
    & listToMaybe

  ((_, trigInd),_,_) <- listToMaybe (traceThisM "lanes" lanes)

  let
    laneInds =
      allLanes
      & map (\z -> z&map (\(_,lid,_) -> lid))
      & concat
      & sort
      & nub

  realLaneInd <- laneInds^?ix y

  return $ m&curTriggerVariation.trigEntryLanes.ix (traceThisM "laneind" $ realLaneInd).laneEntries
    %~ modMapRange (traceThisM "trigind" trigInd) trigInd f
  where
    offX = m^._2.laneViewOffset._1
    offY = m^._2.laneViewOffset._2


onGridPress :: ControllerFunctions -> Bool -> Bool -> Int -> Int -> SeqModel -> SeqModel
onGridPress funcs shiftHeld doubleTapped x y m =
  if doubleTapped then
    m
    & editCompactNote funcs shiftHeld x ((funcs&viewHeight) - 1 - y) (\e -> if isJust e then Nothing else Just $ m&defaultNewEntry)
  else
    m
    -- & (if compact then id else editTrigger funcs shiftHeld offsX offsY)
    & if shiftHeld then id
      else if compact then
        compactSelect funcs x ((funcs&viewHeight) - 1 - y)
      else
        select funcs offsX offsY
    where
      offsX = m^._2.laneViewOffset._1 + x
      offsY = y - m^._2.laneViewOffset._2
      compact = m^._2.laneViewCompact


-- editTrigger :: ControllerFunctions -> Bool -> M.Key -> Int -> SeqModel -> SeqModel
editTrigger funcs shiftHeld x y editFun m = fromMaybe m $ do
  laneInd <- m&getOccupiedLaneIndFromY funcs y
  return $ m&curTriggerVariation.trigEntryLanes.ix laneInd.laneEntries
    %~ modMapRange x x editFun
  -- where
    -- edit mbcurEntry =
    --   if shiftHeld && (isJust mbcurEntry) then
    --     Nothing
    --   else
    --     let e =
    --           if shiftHeld then ContinuationEntry
    --           else m&defaultNewEntry
    --     in
    --       Just e


getOccupiedLaneIndFromY :: ControllerFunctions -> Int -> SeqModel -> Maybe Int
getOccupiedLaneIndFromY funcs y m =
  m&getOccupiedLanes
  & drop ind
  & listToMaybe
  & fmap snd
  where
    ind = ((funcs&viewHeight) - 1 - y)



compactRender :: AppModeRender
compactRender appInf input m funcs =
  m
  & getAllLaneActiveNotes offX offY
  & fromMaybe []
  & take (funcs&viewWidth)
  & mapInd (,)
  & map draw
  & concat
  where
    offX = m^._2.laneViewOffset._1
    offY = m^._2.laneViewOffset._2
    draw (ents, screenX) =
      ents
      & map(\((trig, trigInd), laneInd, laneScreenY) ->
        let
          col =
            if (laneInd == (m^._2.curLaneInd)) && (trigInd == (m^._2.selectedTriggerEntry)) then
              StrongAmber
            else
              StrongRed
        in  (GridXY screenX ((funcs&viewHeight) - 1 - laneScreenY), col)
      )


render :: AppModeRender
render appInf input m funcs =
   (RowPos$Side 7, if m^._2.laneViewCompact then StrongRed else NoCol) : (
    if m^._2.laneViewCompact then
      compactRender appInf input m funcs
    else
      m&getOccupiedLanes
      & drop (m^._2.laneViewOffset._2)
      & mapInd (\(lane, origInd) ind -> renderLane lane ind origInd)
      & concat
    )
  where
    offX = m^._2.laneViewOffset._1
    renderLane lane laneRenderInd laneInd =
      lane^.laneEntries
      & sparseListFromMap
      & drop offX
      & mapInd (,)
      & mapMaybe (\(mbEnt, entInd) ->
        mbEnt&fmap(\e ->
          let
            col =
              if (laneInd == (m^._2.curLaneInd)) && (entInd + offX == (m^._2.selectedTriggerEntry)) then
                StrongAmber
              else case e of
                ContinuationEntry -> StrongGreen
                TriggerEntry _ -> StrongRed
          in  (GridXY entInd ((funcs&viewHeight) - 1 - laneRenderInd), col)
        )
      )




renderPlayPos :: AppModeRender
renderPlayPos appInf inp m funcs =
  m&getAllLaneActiveNotes offX offY
  & maybe [] (
      take ((funcs&viewWidth)+1)
      >>> map listToMaybe
      >>> renderPos
    )
  where
    offX = m^._2.laneViewOffset._1
    offY = m^._2.laneViewOffset._2

    renderPos :: [Maybe ((TriggerData, Int), Int, Int)] -> [RenderEntry]
    renderPos xs =
      m&curPlayingTriggerIndex (appInf^.appPlayerState&getPlayerStep)
      & maybe [] (\playIndex ->
        xs
        -- & mapInd (\x i -> x&maybe Nothing (const $ (x,i)))
        & mapInd (\x i -> x&fmap(\v ->(v,i)))
        & catMaybes
        & reverse
        & find(\(((_,i),_,_), screenInd) -> i <= playIndex)
        & fmap (\(((_,_),_,_), screenInd) -> [
           (GridXY screenInd 0, StrongGreen)
        ])
        & fromMaybe []
      )



