{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module SeqEditMode(
  minOctave
  ,seqEditMode
  ,SeqEditMode.update
  ,SeqEditMode.update'
  ,noteNumFromSide
  ,editNoteOctave
  ,editNoteNum
  ,viewNote
  ,renderPlayPos
  ) where

import AppMode
import Control.Lens((&), ix, (^.), (%~), (^?), (.~), (-~), _1, _2, Getter, (^?))
import Sequencer
import Input
import ControllerCommon
import Data.Maybe
import Control.Arrow
import Misc
import SeqConstants
import ControllerFunctions
import qualified Data.IntMap.Strict as M
import qualified Data.Map.Strict as MM
import SeqEditActions
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List
import SeqQueries
import Data.Monoid ((<>))
import LaneActions
import Data.Foldable (toList)
import Player
import Transport


-- minOctave isLock = if isLock then 3 else 1
minOctave = 3


seqEditMode :: AppMode
seqEditMode = offsetterMode seqEditViewOffset $ runModes [
  ](
    contOptionToggler (RowPos $ Side 6) seqEditShowAll
    <> recToggleMode (RowPos $ Side 7)
    <> runModeIfDown [RowPos $ Side 2] allNotesLengthSetter nopMode
      <> runModeIfDown [RowPos $ Side 1] (loopPointSetter<>laneFromNoteSetter) nopMode
      <> contOptionTogglerWith (RowPos$Side 5) seqEditMoverActive noteMover nopMode
      <> playPosSetter (RowPos$Side 0)
      <> (
        runModes [
          runModeIfDown [RowPos$Side 0] velocitySetter
          ,runModeIfDown [RowPos$Side 4] variationSelector
        ] seqPageSetter
      )
    <> meloSeqEditModeCore
  )


playPosSetter key = meloSeqEditModeCore {
  _update = actionUpdate $ \_ inp m funcs ->
    if inp&keyDown key then do
      y <-
        if inp^.isDoubleTap then
          Just $ m^._2.seqEditViewOffset._2
        else case inp^.ev of
          InDown(GridXY _ y) ->
            Just y
          _ -> Nothing
      (curInd, _) <- selectedTrackEntry m
      return [DoTransport . SetSongPos $
          curInd + y *(funcs&viewWidth)
        ]
    else
      Nothing
}



seqPageSetter :: AppMode
seqPageSetter = nopMode{
  _update = simpleUpdate $ \_ inp m funcs ->
    case inp^.ev of
      InDown(RowPos(Top x)) -> Just $ m&_2.seqEditViewOffset._2 .~ (x*(funcs&viewHeight))
      _ -> Nothing
  ,_render = \_ inp m funcs ->
    [(RowPos$Top ((m^._2.seqEditViewOffset._2) `div` (funcs&viewHeight)), StrongRed)]
}


allNotesLengthSetter :: AppMode
allNotesLengthSetter = nopMode{
  _update = simpleUpdate $ \_ inp m funcs ->
    case inp^.ev of
      InDown(RowPos(Top x)) ->
        if m^._2.seqEditShowAll then
          Just $ m&curLanes %~ (M.map (setLaneEntryLengths x))
        else
          Just $ m&curTrigSeqLane %~ (setLaneEntryLengths x)
      _ -> Nothing
}


variationSelector :: AppMode
variationSelector = nopMode{
  _update = simpleUpdate $ \_ inp m funcs ->
    if inp&keyDown Shift then
      case inp^.ev of
        InDown(RowPos(Top 0)) -> Just $ m&startNewSeqVariation
        InDown(RowPos(Top 1)) -> Just $ m&bringSeqVariationToFront
        InDown(RowPos(Top 2)) -> Just $ m&newerSeqVariation
        InDown(RowPos(Top 3)) -> Just $ m&olderSeqVariation
        InDown(RowPos(Top 4)) -> Just $ m&pushSeqVariationToBack
        _ -> Nothing
    else
      case inp^.ev of
        InDown(RowPos(Top x)) ->
          let mbInd =
                m^?curTrackEntry >>= (\te ->
                  m^?_1.triggerSeqGroups.ix (te^.seqId._1) >>= (\seqs ->
                    seqs^.trigGroupSeqs
                    & toList
                    & drop x
                    & listToMaybe
                    & fmap fst
                ))
          in
            mbInd&fmap(\ind ->
                m&curSeqId._2 .~ ind
              )
        _ -> Nothing

  ,_render = \_ inp m funcs ->
    let
      curVariation =
        m^?curTrackEntry >>= (\te ->
          m^?_1.triggerSeqGroups.ix (te^.seqId._1) >>= (\seqs ->
            let mbInd =
                  seqs^.trigGroupSeqs
                  & toList
                  & findIndex(\(i, _)->i == te^.seqId._2)
            in
              mbInd&fmap(\ind -> [(RowPos$Top ind,StrongRed)])
          )
        )

      allVariations =
        m^?curTriggerSeq&fmap(\sq ->
          [0..((sq^.trigGroupSeqs&length)-1)]&map(\i ->
            (RowPos$Top i, StrongRed)
          ))
    in
      (if inp&keyDown Shift then allVariations else curVariation)
      &fromMaybe []
}


noteMover :: AppMode
noteMover = nopMode {
  _update = simpleUpdate $ \_ inp m funcs ->
    let
      mover step =
        if m^._2.seqEditShowAll then
          m&curLanes %~ (M.map (moveLaneNotes (funcs&viewWidth) step))
        else
          m&moveCurNotes (funcs&viewWidth) step

    in case inp^.ev of
      InDown (RowPos (Top 0))-> Just $ mover ((-1)*(funcs&viewHeight))
      InDown (RowPos (Top 1))-> Just $ mover (funcs&viewHeight)
      InDown (RowPos (Top 2))-> Just $ mover (-1)
      InDown (RowPos (Top 3))-> Just $ mover 1
      _ -> Nothing
}


laneFromNoteSetter :: AppMode
laneFromNoteSetter = nopMode{
  _update = simpleUpdate $ \_ inp m _ ->
    case inp^.ev `debug` "laneFromNoteSetter" of
      InNote (NoteDown (k,_):|_) ->
        Just $
          m
          & copyLaneNotes (not $ inp&keyDown Shift) (m^._2.curLaneInd) k
          & _2.curLaneInd .~ k
      _ -> Nothing
}


velocitySetter :: AppMode
velocitySetter = nopMode{
  _update = simpleUpdate $ \_ inp m _ ->
    case inp^.ev of
      InDown(RowPos(Top x)) ->
        -- Update velocity for current trigger, if any
        Just $ m&curLaneEntry %~ (\case
              ContinuationEntry -> ContinuationEntry
              TriggerEntry t ->
                TriggerEntry $ t&vel .~ velForStep x
            )
      _ -> Nothing

  ,_render = \_ _ m funcs ->
    m^?curLaneEntry&maybe [] (\case
      ContinuationEntry -> []
      TriggerEntry t ->
        let ind = velStepFromVal $ t^.vel
        in [ (RowPos$Top ind, StrongRed)]
    )
}


meloSeqEditModeCore = AppMode renderPlayPos  SeqEditMode.update renderTriggers


triggerEditUpdate :: ControllerFunctions -> Bool -> InputState -> SeqModel -> SeqModel
triggerEditUpdate funcs selectOnToggle input oldModel =
  oldModel
  \>(case input^.ev of
    InDown cell@(GridXY _ _) ->
      let
        ind = (funcs&indexFromCell) cell
      in
        if (traceThis ind) < (traceThis len) then
          (editCurLane $ laneEntries %~ toggleMapEntry ind (oldModel&defaultNewEntry))
          >>> (if selectOnToggle then select funcs ind else id)
          >>> curTrigSeqLane %~ (\lane ->
              lane&laneEntries %~
                deleteContinuationsFrom  ind (lane^.laneLoopLen)
            )
        else
          id
    _ -> id
    )
  where
    maybeTrigSeq = oldModel^?curTriggerVariation
    len = getSeqLen maybeTrigSeq


getSeqLen :: Maybe TriggerSeq -> Int
getSeqLen maybeTrigSeq =
  maybeTrigSeq&maybe 0 getSeqLength


select :: ControllerFunctions -> Int -> SeqModel -> SeqModel
select funcs ind model=
  let
    mNewIndex =
      model&curSeqAndLane >>= (\(sq, lane) ->
         lane^?laneEntryFromLane ind >>= (\case
            ContinuationEntry -> Nothing
            TriggerEntry _ ->
              Just $ min ind (sq&getSeqLength)
         )
      )
  in
    mNewIndex&maybe model (\i ->
      model
      & _2.selectedTriggerEntry .~ i
      & updateLastTrigEntry
    )



selectLane :: Int -> ControllerFunctions ->  SeqModel -> SeqModel -> SeqModel
selectLane y funcs m =
  (setActiveLane v)
  .(_2.laneViewOffset._2 %~ \orig ->
      m&getOccupiedLanes
      & findIndex (\(_,i)->i==v)
      & fromMaybe orig
      & (\x -> max 0 (x-((funcs&viewHeight) `div` 2)))
    )
  where
    v = (baseDrumNote + y)



onGridPress :: ControllerFunctions -> Bool -> Cell -> InputState -> SeqModel -> SeqModel
onGridPress funcs selectOnToggle cell input model =
  if model^._2.seqEditShowAll then
    onGridPressAll funcs selectOnToggle cell input model
  else
    onGridPressSingle funcs selectOnToggle cell input model


onGridPressSingle :: ControllerFunctions -> Bool -> Cell -> InputState -> SeqModel -> SeqModel
onGridPressSingle funcs selectOnToggle cell input model =
  let
    ind = (funcs&indexFromCell) cell
    -- len = getSeqLen (model^?curTriggerVariation)
  in
    if input&keyDown Shift then
        model
        & curTrigSeqLane %~ (\lane ->
          lane&laneEntries %~(
            toggleMapEntry ind ContinuationEntry
            >>>(\mp ->
              if isNothing (mp^?ix ind) then
                deleteContinuationsFrom  ind (lane^.laneLoopLen) mp `debug` "removing conts"
              else
                addContinuationsTo ind mp `debug` "adding conts"
            )
        ))
    else
      let
         selCell = model&curSelectedCell funcs
      in
        if (input^.ev /= InDown selCell) && (input&keyDown selCell) then
          model
          \> laneEntry ind %~ (
            model^?curLaneEntry
            \> maybe id (\case
                ContinuationEntry -> const $ ContinuationEntry
                TriggerEntry t ->
                  const $ TriggerEntry $ t&delay .~ (t^.delay))
          )
        else if ((isNothing $ model^?laneEntry ind) || (input ^.isDoubleTap && (model^._2.selectedTriggerEntry) == ind)) then
          triggerEditUpdate funcs selectOnToggle input model
        else
          select funcs ind model


findLaneForIndex :: Int -> SeqModel -> Maybe (Int, LaneEntries)
findLaneForIndex ind model = do
    trigSeq <- model^?curTriggerVariation
    (trigSeq^.trigEntryLanes)
      \> M.assocs
      \> map (\(laneInd,lane) ->
          (laneInd, expandLane (trigSeq&getSeqLength) lane)
        )
      \> find (\(laneInd, entries) ->
          isJust $ M.lookup ind entries
        )


onGridPressAll :: ControllerFunctions -> Bool -> Cell -> InputState -> SeqModel -> SeqModel
onGridPressAll funcs selectOnToggle cell input model =
  let
    pressedInd = (funcs&indexFromCell) cell
  in
    model
    \> findLaneForIndex pressedInd
    \> fmap(\(laneInd, _) ->
        model
        \> setActiveLane laneInd
        \> _2.selectedTriggerEntry .~ pressedInd
    )
    \> fromMaybe model




loopPointSetter :: AppMode
loopPointSetter = nopMode{
  _update = simpleUpdate $ \_ inp model funcs ->
    case inp^.ev of
      InDown cell@(GridXY _ _) ->
        let ind = (funcs&indexFromCell) cell
        in
          if inp^.isDoubleTap then
            setAllLoopPoints (Just ind) model `debug` "Set single loop point"
          else
            setCurLoopPoint (Just ind) model`debug` "Set all loop points"
      InDown Shift ->
          if inp^.isDoubleTap then
            setAllLoopPoints Nothing model `debug` "Reset single loop point"
          else
            setCurLoopPoint Nothing model`debug` "Reset all loop points"
      _ -> Nothing
  ,_render = \_ _ model funcs ->
    model&curSeqAndLane
    &fmap (\(sq, lane)->
      renderEndMarker funcs True lane (sq&getSeqLength) (model^._2.selectedTriggerEntry)
    )&fromMaybe []
    -- model^?curTriggerVariation.seqFullLength&maybe [] (\len ->
    --   [(((funcs&gridFromIndex) len), StrongAmber)]
    -- )
}


setAllLoopPoints :: Maybe Int -> SeqModel -> Maybe SeqModel
setAllLoopPoints mbNewLoopLen model =
  (model^?curTriggerVariation)
  & fmap (\sq ->
    let
      maxLen = sq&getSeqLength
      newLen = mbNewLoopLen&fromMaybe maxLen
    in
      model
      & _2.newLaneLoopLen .~ newLen
      & curLanes %~ M.map (setLaneLoopPoint newLen)
  )


setCurLoopPoint :: Maybe Int -> SeqModel -> Maybe SeqModel
setCurLoopPoint mbNewLoopLen model =
  (model^?curTriggerVariation)
  & fmap (\sq ->
    let
      maxLen = sq&getSeqLength
      newLen = mbNewLoopLen&fromMaybe maxLen
    in
      model
      & curTrigSeqLane %~ setLaneLoopPoint newLen
  )


setLaneLoopPoint :: Int -> TrigSeqLane -> TrigSeqLane
setLaneLoopPoint newLoopLen lane =
  lane&setNewLoopPoint newLoopLen


setNewLoopPoint :: Int -> TrigSeqLane -> TrigSeqLane
setNewLoopPoint newLoopLen lane =
  if newLoopLen <= 0 then
    lane
  else
    lane
    & laneEntries %~ (M.filterWithKey(\k v -> k < newLoopLen))
    & laneLoopLen .~ newLoopLen


update = SeqEditMode.update' True


activeEditNote :: SeqModel -> NoteEntry
activeEditNote m =
  m&trackNumToNote (m^._2.curLaneInd)


getActions :: ControllerFunctions -> InputState -> SeqModel -> [ModeAction]
getActions funcs input model =
  case input^.ev of
    InUp _ -> [StopPlayNote n]
    _ ->
      if noAct then []
      else case input^.ev of
        -- InNoteDown k v -> [PlayNote k v]
        InNote _ -> []
        InDown cell ->
          if input&keyDown (model&curSelectedCell funcs) then
            case cell of
              GridXY _ _ ->
                if model^._2.seqEditShowAll then
                  let ind = (funcs&indexFromCell) cell
                  in fromMaybe [] $ do
                    (laneInd, _) <- findLaneForIndex ind model
                    return $ [PlayNoteOneShot laneInd defaultVelocity]
                else
                  []
              _ -> []
          else
            case cell of
              RowPos(Top 7) -> []
              RowPos(Side 7) -> []
              RowPos(Top _) -> [PlayNote n defaultVelocity]
              RowPos(Side _)  ->[PlayNote n defaultVelocity]
              _ -> []
        _ -> []

    where
      n = model&mapTriggerNote selInd (model&activeEditNote)
      selInd = model^?curLaneEntry&fmap(const $ model^._2.selectedTriggerEntry)
      noAct =
        case input^.ev of
          InDown(RowPos(Side _)) -> not $ input&keyDown (RowPos(Top 7))
          InUp(RowPos(Side _)) -> not $ input&keyDown (RowPos(Top 7))
          InNote _ -> False
          _ -> (input&keyDown Shift) || (input&keyDown (RowPos(Top 7)))


update' selectOnToggle _ input model funcs =
  modFun
  \> fmap(\f ->
      ((), UpdateSingle, f model, getActions funcs input (f model))
    )
  where
    modFun :: Maybe (SeqModel -> SeqModel)
    modFun =
      if (input&keyDown (RowPos$Top 7)) && (input&keyDown (RowPos$Top 6)) then
        -- Temporary mode for stuff I don't know where to put..
        case input^.ev  of
          InDown (RowPos (Side 0))-> Just $ curLaneEntry._TriggerEntry %~ (triggerDouble %~ not)
          _ -> Nothing
      else if (not $ input&keyDown Shift) && (input&keyDown ( RowPos $ Top 7 )) then -- using button 7 as Shift2
        case input^.ev of
          InDown(RowPos(Side 7)) -> Nothing -- rec mod
          _ -> Nothing
      else if (keyDown Shift input) then
        case traceThisM "SHIFT DOWN" $ input^.ev  of
          InDown cell@(GridXY _ _) ->
            Just $ onGridPress funcs selectOnToggle cell input
          InNote (x:|_) -> case x of
            NoteDown (k,v) -> Just (\m ->
                m&modActiveNote (Just v, const$ (m&trackNumToNote k))
              )
            NoteUp _ -> Nothing
          _ ->
            Nothing
      else
        case traceThisM "SHIFT NOT DOWN" $ input^.ev of
          InNote (x:|_) -> Just (\m ->
            case x of
              NoteDown (k,v) ->(
                if input&keyDown (model&curSelectedCell funcs) then
                  m&modActiveNote (Just v, const (m&trackNumToNote k))
                else
                  m&setActiveLane k
                ) & _2.lastSelTrigger.vel .~ v
              NoteUp _ -> m
            )
          InDown cell ->
            case cell of
              GridXY _ _ -> Just $ onGridPress funcs selectOnToggle cell input
              RowPos(Top 7) -> Nothing -- using button 7 as Shift2
              _ -> Nothing
          _ ->
            Nothing


modActiveNote :: (Maybe Int, (NoteEntry -> NoteEntry)) -> SeqModel -> SeqModel
modActiveNote (mbNewVel, f) m =
  m&setActiveLane (m&trackNoteToNum (f (m&curLaneNote)))


editNoteNum :: ControllerFunctions -> SeqModel -> Int -> NoteEntry -> NoteEntry
editNoteNum funcs model n note =
  model&trackNumToNote v
  where
    transNote = model&viewNote funcs note
    v = (model&trackNoteToNum (NoteEntry 0 minOctave)) + noteToNum' ((funcs&rowPosTopCount)-1) (transNote&nNum.~n)


editNoteOctave :: ControllerFunctions -> SeqModel -> Int -> NoteEntry -> NoteEntry
editNoteOctave funcs model newOct note =
  model&trackNumToNote (octV + transNote^.nNum)
  where
    octV = model&noteNumFromSide funcs newOct
    transNote = model&viewNote funcs note


noteNumFromSide :: ControllerFunctions -> Int -> SeqModel -> Int
noteNumFromSide funcs y m=
  (m&trackNoteToNum (NoteEntry 0 minOctave)) + (y*((funcs&rowPosSideCount)-1))


renderPlayPos :: AppInfo -> a -> SeqModel -> ControllerFunctions -> [RenderEntry]
renderPlayPos appInf _ m funcs =
  m
  \> curPlayingTriggerIndex (appInf^.appPlayerState&getPlayerStep)
  \> maybe [] (\playIndex ->
      [(((funcs&gridFromIndex) playIndex), StrongAmber)]
    )


renderEndMarker :: ControllerFunctions -> Bool -> TrigSeqLane -> Int -> Int -> [RenderEntry]
renderEndMarker funcs withFullLenMarker lane maxLen selInd =
  fullLen ++ end
  where
    entLen = getLaneLength lane

    end =
      if entLen == selInd then []
      else
        [(((funcs&gridFromIndex) entLen), StrongGreen)]

    fullLen =
      if withFullLenMarker then
        [(((funcs&gridFromIndex) maxLen), StrongAmber)]
      else []


viewNote :: ControllerFunctions -> NoteEntry -> SeqModel -> NoteEntry
viewNote funcs n m =
  let
    v = m&trackNoteToNum (n&octave -~ minOctave)
  in
    numToNote' ((funcs&rowPosTopCount) - 1) v


renderLane :: ControllerFunctions -> Bool -> Int -> TrigSeqLane -> InputState -> SeqModel -> MM.Map Cell Color
renderLane funcs withSelectionMarker maxLen lane input model =
  (sparseListFrom' maxLen $ expandLane maxLen lane)
  & gridRender funcs getColor
  & foldl' (\accum (k,v) ->
      accum&MM.insert k v
    ) mempty
  where
    getColor ind =
      lane^?laneEntryFromLane ind >>= (\entry ->
        if ind == (model^._2.selectedTriggerEntry) && withSelectionMarker then
          Just StrongAmber
        else if ind < (getLaneLength lane) then
          if (isJust $ entry^?_ContinuationEntry) && (not $ input&keyDown (RowPos$Top$7)) then
            Just StrongGreen
          else
            Just StrongRed
        else if(isNothing $ entry^?_ContinuationEntry) then
          Just StrongGreen
        else
          Nothing
      )


renderGrid :: ControllerFunctions -> Bool -> InputState -> SeqModel -> [RenderEntry]
renderGrid funcs withSelectionMarker input model=
  model
  \> curSeqAndLane
  \> fmap(\(sq, lane) ->
      renderLane funcs withSelectionMarker (sq&getSeqLength) lane input model
      & MM.assocs
    )
  \> fromMaybe []


renderGridAllLanes:: ControllerFunctions -> Bool -> InputState -> SeqModel -> [RenderEntry]
renderGridAllLanes funcs withSelectionMarker input model=
  model^?curTriggerVariation
  & fmap(\sq ->
    sq^.trigEntryLanes
    & M.elems
    & map(\lane ->
        renderLane funcs withSelectionMarker (sq&getSeqLength) lane input model
      )
    & MM.unions
    & MM.assocs
  )
  \> fromMaybe []


curNewNoteRender :: ControllerFunctions -> Getter NoteEntry a -> (a -> RowIndex) -> SeqModel -> [RenderEntry]
curNewNoteRender funcs lns rowPosConstruct model =
  [((RowPos $ rowPosConstruct (v^.lns)), StrongRed)]
  where
    (n, _) = model&curNewNote
    v = model&viewNote funcs n


renderSide :: ControllerFunctions -> InputState -> SeqModel -> Maybe TriggerData -> [RenderEntry]
renderSide funcs input model mTrig =
  if input&keyDown Shift then []
  else if (input&keysDown [RowPos$Top 7, RowPos$Top 6])
    && (model^?curLaneEntry._TriggerEntry.triggerDouble&fromMaybe False) then
      [(RowPos$Side 0, StrongRed)]
  else
    []
  where
    maybeInd =
      if not $ input&keyDown (RowPos$Top 7) then
        Just (model^._2.curLaneInd - baseDrumNote)
      else
        Nothing


renderTriggers :: a -> InputState -> SeqModel -> ControllerFunctions -> [RenderEntry]
renderTriggers _ input model funcs =
  model
  \> curSeqAndLane
  \> fmap (\case {(sq, lane) ->
      concat [
        gridRenderFun funcs (not withFullLenMarker) input model
        , renderSide funcs input model (mTrig>>=(^?_TriggerEntry))
      ]
      where
        mTrig = lane^?laneEntryFromLane (model^._2.selectedTriggerEntry)
        withFullLenMarker = input&keyDown (RowPos$Top 2)
        selInd = model^._2.selectedTriggerEntry
    })
  \> fromMaybe []

  where
    gridRenderFun =
      if model^._2.seqEditShowAll then
        renderGridAllLanes
      else
        renderGrid
