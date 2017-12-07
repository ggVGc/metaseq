{-# LANGUAGE Rank2Types #-}

module ArrangeMode2 where


import Data.Monoid ((<>))
import AppMode
import Control.Lens
import Sequencer
import Input
import ControllerCommon
import SeqSelectMode
import Misc
import Data.Maybe
-- import RootNoteMode
-- import Control.Monad
import qualified Data.Sequence as S
import ControllerFunctions
import ArrangementActions
import GenericModes(gridSelectMode)
import Data.List (find, nub, sort)
import SeqConstants (sequenceDefaultLen)
import Transport
import Player
import Data.IntMap (lookupLE, lookupGT, lookupLT)
import qualified Data.IntMap as M
import Data.Foldable(toList)
import RootNoteMode



arrangeMode :: AppMode
arrangeMode =
  overrideUpdateFlag UpdateLastTouched $
    runModes [
      -- arrViewNotesToggler (RowPos$Top 1)
      runModeIfDown [RowPos$Top 5] (seqVariationSelector StrongRed)
      , runModeIfDown [RowPos$Top 6] (seqSelectMode StrongGreen)
      , runModeIfDown [RowPos$Top 7] channelSelector
    ] (
        (runModes [
          runModeIfDown [RowPos$Side 5] entryLengthSetter
          ,runModeIfDown [RowPos$Side 6] playLoopSetter
          ,runModeIfDown [RowPos$Side 7] transportMode
          ,runModeIfDown [RowPos$Side 4] (
            masterToggler (RowPos$Top 0)
            <> playbackToggler (RowPos$Top 1)
            <> (bpmChanger (RowPos$Top 2) (RowPos$Top 3)))
          ] (
              recToggleMode (RowPos $ Side 3)
              <> viewOffsetSelector (arrViewOffset2._2))
            )
        <> arrMode2
        <> runModeIfHeld [RowPos$Top 0]
              (viewMover (arrViewOffset2._1) Top (Nothing, Nothing, Just 1))
              (viewMover (arrViewOffset2._1) Top (Just 0,Just 1, Nothing))
      )



arrMode2 :: AppMode
arrMode2 = nopMode{
    _update = simpleUpdate $ ArrangeMode2.update
    ,_render = ArrangeMode2.render
    ,_renderPlayPosition = ArrangeMode2.renderPlayPos
    -- ,_renderPlayPosition = ArrangeMode2.render
  }


getPlayPos :: AppInfo -> SeqModel -> Int
getPlayPos appInf model =
  (appInf^.appPlayerState&getPlayerStep) `div` (model^._2.arrModeStepsPerEntry)


renderPlayPos :: AppModeRender
renderPlayPos appInf inp model funcs =
  if x>=0 && x<(funcs&viewWidth) then
    [(GridXY x ((funcs&viewHeight)-1), col)]
  else []
  where
    col = WeakAmber
    (offsetX, _) = model^._2.arrViewOffset2
    playCell = model&getPlayPos appInf
    x = playCell - offsetX


select :: Int -> Int -> SeqModel -> SeqModel
select trackInd trackPos model =
  model&
    (_2.curTrackIndex .~ trackInd)
    .(_2.selectedTrackPos .~ trackPos)


toggleEntry :: Int -> Int -> ControllerFunctions -> SeqModel -> SeqModel
toggleEntry trackInd trackPos funcs model =
  model
  \> toggleTrackEntry trackInd trackPos
  \> (\m -> fromMaybe m $ do
    tracks <- m^?curArrangement
    track <- tracks^?ix trackInd
    (prevStartPos, prevEnt) <- track^.trackEntries&lookupLT (trackPos-1)
    setEntryLength (min trackPos (prevStartPos + (prevEnt^.teLength))) trackInd funcs m
  )
  \>(\m -> fromMaybe m $ do
      tracks <- m^?curArrangement
      track <- tracks^?ix trackInd
      (nextStartPos, ent) <- track^.trackEntries&lookupGT trackPos
      setEntryLength (min nextStartPos (trackPos + (ent^.teLength))) trackInd funcs m
  )


update :: SimpleUpdate
update appInf inp model funcs =
  case inp^.ev of
    InDown Shift ->
      if inp^.isDoubleTap then
        Just $ model&focusSelected funcs
      else
        Nothing
    InDown(GridXY x y) -> do
      tracks <- model^?curArrangement
      let
        entInds = trackEntInds tracks (model^._2.arrModeStepsPerEntry)
        trackInd = y+offsetY
        trackPos = entInds^?ix (offsetX+x)&fromMaybe 0
      Just $
        if inp^.isDoubleTap then
          model&toggleEntry trackInd trackPos funcs
        else
          model
          \> select trackInd trackPos

    InDown(RowPos(Top x)) ->
      if inp&keyDown Shift then
        if x == 0 then
          Just $
            model
            \> _2.arrModeStepsPerEntry %~ (\i -> min (i * 2) sequenceDefaultLen)
            \> refocus
        else if x == 1 then
          Just $
            model
            \> _2.arrModeStepsPerEntry %~ (\i -> max 1 (i `div` 2))
            \> refocus
        else Nothing
      else Nothing
    _ -> Nothing
  where
    (offsetX, offsetY) = model^._2.arrViewOffset2
    curCenterVal =
      let
        tracks = model^?curArrangement \> fromMaybe mempty
        inds = trackEntInds tracks (model^._2.arrModeStepsPerEntry)
      in
        inds^?ix (model^._2.arrViewOffset2._1) \> fromMaybe 0

    refocus m =
      m&focusOn curCenterVal funcs


focusOn :: Int -> ControllerFunctions -> SeqModel -> SeqModel
focusOn pos funcs m =
  m
  \> _2.arrViewOffset2._1 .~ max 0 newFocusInd
  where
    tracks = m^?curArrangement \> fromMaybe mempty
    inds = trackEntInds tracks (m^._2.arrModeStepsPerEntry)
    newFocusInd =
      inds
      \> takeWhile (\x -> x < pos)
      \> length


focusSelected :: ControllerFunctions -> SeqModel -> SeqModel
focusSelected funcs m =
  focusOn (m^._2.selectedTrackPos) funcs m


render :: AppModeRender
render appInf inp model funcs = fromMaybe [] $ do
  tracks <- model^?curArrangement
  let trackRends = renderTracks tracks (funcs&viewWidth) (funcs&viewHeight) (model&getPlayPos appInf) model
  return trackRends


allEntInds :: Foldable t => t ArrTrack -> [M.Key]
allEntInds tracks =
  tracks
  \> toList
  \> map (\t -> M.keys $ t^.trackEntries)
  \> concat
  \> sort
  \> nub

trackEntInds :: Foldable t => t ArrTrack -> M.Key -> [M.Key]
trackEntInds tracks stepsPerEntry =
  [0,stepsPerEntry .. mn]
  <> (
    (([mn,mn+stepsPerEntry .. mx] <> entInds)
    \> sort
    \> nub
  )
  <> [mx+stepsPerEntry, (mx+stepsPerEntry*2) ..])
  \> nub
  where
    entInds = allEntInds tracks
    mn = if null entInds then 0 else minimum entInds
    mx = if null entInds then mn else maximum entInds


trackEntIndsOnScreen :: Foldable t => t ArrTrack -> ControllerFunctions -> SeqModel -> [M.Key]
trackEntIndsOnScreen tracks funcs m =
  trackEntInds tracks stepsPerEntry
  \> drop offsetX
  where
    stepsPerEntry = m^._2.arrModeStepsPerEntry
    contState = m^._2
    offsetX = contState^.arrViewOffset2._1


renderTracks :: S.Seq ArrTrack -> Int -> Int -> Int -> SeqModel -> [RenderEntry]
renderTracks tracks screenWidth screenHeight playPos m =
  tracks
  \> S.drop offsetY
  \> S.mapWithIndex (\trackInd track ->
      let
        mbSelInd =
          if trackInd + offsetY == (contState^.curTrackIndex) then
            Just (contState^.selectedTrackPos)
          else
            Nothing

        cols =
          renderTrackLazy stepsPerEntry track sharedMod mbSelInd inds
          -- \> mapInd(\col ind ->
          --   if playPos == offsetX + ind then
          --     col&fmap makeWeakCol
          --   else col
          -- )
      in
        cols
        \> mapInd(\col i -> col&fmap (\c -> (GridXY i trackInd, c)))
        \> catMaybes
    )
  \> S.take screenHeight
  \> concat
  where
    sharedMod = m^._1
    contState = m^._2
    offsetX = contState^.arrViewOffset2._1
    offsetY = contState^.arrViewOffset2._2
    stepsPerEntry = m^._2.arrModeStepsPerEntry
    inds =
      trackEntInds tracks stepsPerEntry
      \> drop offsetX
      \> take screenWidth


renderTrackLazy :: Int -> ArrTrack -> SharedModel -> Maybe Int -> [Int] -> [Maybe Color]
renderTrackLazy stepsPerEntry track sharedMod mbSelectedTrackPos inds =
  inds
  \> map (colFromIndex track sharedMod mbSelectedTrackPos)


colFromIndex :: ArrTrack -> SharedModel -> Maybe Int -> Int -> Maybe Color
colFromIndex track sharedMod mbSelectedTrackPos trackPos = do
  (teStartPos, te) <- track^.trackEntries&lookupLE trackPos
  if teStartPos + te^.teLength > trackPos then
    if mbSelectedTrackPos&maybe False checkSelected then
      return StrongAmber
    else if teStartPos == trackPos then
      return StrongGreen
    else
      return StrongRed
  else
    Nothing

  where
    checkSelected selPos = fromMaybe False $ do
      (x,_) <- track^.trackEntries&lookupLE selPos
      return $ x==trackPos


viewOffsetSelector :: Lens' ControllerState Int -> AppMode
viewOffsetSelector lns = nopMode{
  _update = simpleUpdate $ \_ inp m funcs ->
    case inp^.ev of
      InDown(RowPos(Side x)) ->
        Just $ m&_2.lns.~ x*(funcs&viewHeight)
      _ -> Nothing

  ,_render = \appInf _ m funcs ->
    let
      h = funcs&viewHeight
      offs = m^._2.lns
      ind = offs `div` h
    in [ (RowPos $ Side ind, StrongRed)]
}



viewMover :: Lens' ControllerState Int -> (Int -> RowIndex) -> (Maybe Int, Maybe Int, Maybe Int) -> AppMode
viewMover lns inpConstructor keys = nopMode{
  _update = simpleUpdate $ \appInf inp m funcs ->
    [keys^._1, keys^._2, keys^._3]
    & mapInd(,)
    & find (\(k,_) -> (isJust k) && (k&fmap inpConstructor)== getDownRowPos (inp^.ev))
    >>= (\(_,i)->
        case i of
          0 -> Just $
            m
            \> _2.lns%~ \x -> max 0 $ x-2
          1 -> Just $
            m
            \> _2.lns%~ \x -> max 0 $ x+2
          2 -> Just $ m&focusPlayPosCurrentTrack funcs appInf
          _ -> Nothing
      )
}


playLoopSetter :: AppMode
playLoopSetter = nopMode{
  _update = \appInf inp m funcs ->
    case inp^.ev of
      InDown(GridXY x y) ->
        let
          tracks = m^?curArrangement \> fromMaybe mempty
          inds = trackEntIndsOnScreen tracks funcs m
          end =  inds^?ix (x+1) \> fromMaybe 0
          start =  inds^? ix x \> fromMaybe 0
          curStart = appInf^.appPlayerState&playerGetLoopStart
          curEnd =  appInf^.appPlayerState&playerGetLoopEnd

          acts =
            if inp^.isDoubleTap then
              [SetLoopBegin start, SetLoopEnd end]
            else if inp&keyDown (RowPos$Side 7) then
              [SetLoopBegin start]
            else if end > curStart then
              [SetLoopEnd end]
            else if start <curStart then
              [SetLoopBegin start]
            else
              []

          outActs = map DoTransport acts
          in
            if null acts then Nothing
            else
              Just ((), UpdateSingle, m, outActs)
      _ -> Nothing

  ,_render = \appInf _ m funcs ->
    [0..(funcs&viewHeight)]&concatMap(
        drawLoopPointsForTrack appInf m funcs
      )
}


drawLoopPointsForTrack
  :: AppInfo -> SeqModel -> ControllerFunctions -> Int -> [RenderEntry]
drawLoopPointsForTrack appInf m funcs screenTrackInd = fromMaybe [] $ do
  let
    trackInd = screenTrackInd + (m^._2.arrViewOffset2._2)
  tracks <- m^?curArrangement
  let
    (startTeInd, endTeInd) = getLoopEdges appInf tracks funcs m
  return $ concat [
       startTeInd&maybe [] (\i -> [(GridXY i screenTrackInd, StrongAmber)])
       ,endTeInd&maybe [] (\i -> [(GridXY (i-1) screenTrackInd, StrongAmber)])
    ]

getLoopEdges :: Foldable t => AppInfo -> t ArrTrack -> ControllerFunctions -> SeqModel ->  (Maybe Int, Maybe Int)
getLoopEdges appInf tracks funcs m =
  let
    inds = trackEntIndsOnScreen tracks funcs m
    startPos = appInf^.appPlayerState&playerGetLoopStart
    endPos = appInf^.appPlayerState&playerGetLoopEnd
    curStart = entryFromPos inds startPos ^._2
    curEnd = entryFromPos inds endPos ^._2
    firstVal = inds^?ix 0 \> fromMaybe 0
    lastVal = (reverse inds)^?ix 0 \> fromMaybe 0
  in
    (if startPos < firstVal then Nothing else Just curStart,
    Just curEnd)


entryFromPos :: [Int] -> Int -> (Maybe Int, Int)
entryFromPos inds pos =
  inds
  \> takeWhile (\x -> pos > x )
  \> \ents ->
    (ents&reverse&listToMaybe, length ents)



transportMode :: AppMode
transportMode = nopMode{
  _update = \_ inp m funcs ->
    case inp^.ev of
      InDown(GridXY x _) ->
        let
          tracks = m^?curArrangement \> fromMaybe mempty
          inds = trackEntIndsOnScreen tracks funcs m
          pos =  inds^?ix x \> fromMaybe 0
        in Just ((), UpdateSingle, m, [DoTransport $ SetSongPos pos])
      _ -> Nothing

  ,_render = playLoopSetter&_render
}


channelSelector :: AppMode
channelSelector = gridSelectMode (curTrack.channel) StrongRed


arrViewNotesToggler :: Cell -> AppMode -> AppMode
arrViewNotesToggler key =
  contOptionTogglerWith key arrViewNotesActive (rootNoteMode StrongGreen)


entryLengthSetter :: AppMode
entryLengthSetter = nopMode{
  _update = simpleUpdate $ \_ inp m funcs ->
    case inp^.ev of
      InDown(GridXY x y) -> do
        tracks <- m^?curArrangement
        let
          inds = trackEntIndsOnScreen tracks funcs m
          pos =  inds^?ix (x+1) \> fromMaybe 0
          (_, offsetY) = m^._2.arrViewOffset2

        setEntryLength pos (y+offsetY) funcs m

      _ ->
        Nothing
}


setEntryLength :: Int -> Int -> ControllerFunctions -> SeqModel -> Maybe SeqModel
setEntryLength pos trackInd funcs m = do
  tracks <- m^?curArrangement
  track <- tracks^?ix trackInd
  let
    inds = trackEntInds tracks (m^._2.arrModeStepsPerEntry)
  startPos <-
    inds
    \> takeWhile (\i -> i<pos)
    \> reverse
    \> dropWhile (\i -> isNothing $ track^?trackEntries.ix i)
    \> listToMaybe
  let
    fullNewLen = traceThisM "fullnewlen" $ (traceThisM "pos" pos ) - (traceThisM "startpos" startPos)
    newTeLength = traceThisM "newlen" $ fromMaybe fullNewLen $ do
      (nextStartPos, _) <- track^.trackEntries&lookupGT startPos
      return $ min (nextStartPos - startPos) fullNewLen

  return $
    m&curArrangement.ix trackInd.trackEntries.ix (traceThisM "START POS" startPos).teLength .~ newTeLength



masterToggler :: Cell -> AppMode
masterToggler key = nopMode{
  _update = \appInf inp m funcs ->
    if inp^.ev == InDown key then
      if appInf^.isMaster then
        Just $ ((), UpdateSingle, m, [ClearMaster])
      else
        Just $ ((), UpdateSingle, m, [SetMaster])
    else
      Nothing

  ,_render = \appInf _ _ funcs ->
      if appInf^.isMaster then
        [ (key, StrongRed)]
      else []
}


