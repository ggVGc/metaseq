{-# LANGUAGE TemplateHaskell #-}
module Application where


import ZMidi.Core

import ControllerCommon
import Sequencer
import Modes
import Input
import Control.Lens
import Data.Maybe
import Misc
import AppMode
import ControllerFunctions
import System.Clock
import qualified Data.IntMap.Strict as M
import Data.Foldable(toList)
import qualified Data.Sequence as S

maxUndo = 20

outMidiPath = "out.mid"


data AppModel = AppModel {
  _seqModels :: [SharedModel]
  ,_seqIndex :: Int
  ,_undoPushActive :: Bool
} deriving(Eq)

defaultAppModel = AppModel [defaultSharedModel] 0 True

makeLenses ''AppModel


getUpdatedLanes :: M.IntMap TrigSeqLane -> M.IntMap TrigSeqLane -> [(Int, TrigSeqLane)]
getUpdatedLanes oldLanes newLanes =
  zip (sparseListFromMap oldLanes) (sparseListFromMap newLanes)
  & imap (,)
  & mapMaybe (\(ind, (mbOld, mbNew)) ->
    if mbOld == mbNew then Nothing
    else
      mbNew >>= (\new ->
          if isJust mbOld then
            Just (ind, new)
          else
            Nothing
      )
  )

getUpdatedTrigSeqs :: [(Int, TriggerSeq)] -> [(Int, TriggerSeq)] -> [(Int, TriggerSeq, TriggerSeq)]
getUpdatedTrigSeqs old new =
  new
  & mapMaybe (\(ind, newTrig) ->
      let
        mbOldTrig = old&lookup ind
      in
        mbOldTrig&maybe
          -- (Just (ind, newTrig, newTrig)) -- This might not be completely correct, but not sure what an added lane should mean in this context
          Nothing
          (\oldTrig ->
            if oldTrig /= newTrig then Just (ind, oldTrig, newTrig)
            else Nothing
          )
    )


getUpdatedSeqs :: [TriggerSeqVariations] -> [TriggerSeqVariations] -> [(TriggerSeq, TriggerSeq)]
getUpdatedSeqs oldSeqs newSeqs =
  zip oldSeqs newSeqs
  & mapMaybe(\(old, new)->
      let
        updated = getUpdatedTrigSeqs (toList (old^.trigGroupSeqs)) (toList (new^.trigGroupSeqs))
      in
        if null updated then Nothing
        else Just $ map (\(_,oldTrigSeq,newTrigSeq)->(oldTrigSeq, newTrigSeq)) updated
    )
  & concat


getUpdatedTrackEntries :: [ArrTrack] -> [ArrTrack] -> S.Seq TriggerSeqVariations -> S.Seq TriggerSeqVariations -> [Int]
getUpdatedTrackEntries oldTracks newTracks oldSeqs newSeqs =
  zip oldTracks newTracks
  & mapMaybe(\(oldTrack, newTrack) ->
    let
      diffTrackEntries = (oldTrack^.trackEntries) /= (newTrack^.trackEntries)
      diffSeqs =
        newTrack^.trackEntries
        & M.filterWithKey (\ind newTe ->
          let
            mbOldTe = oldTrack^?trackEntries.ix ind
          in
            mbOldTe&maybe False (\oldTe -> fromMaybe False $ do
              new <- newSeqs^?trigSeqVariation (newTe^.seqId)
              old <- oldSeqs^?trigSeqVariation (oldTe^.seqId)
              -- let fixedOld = old{_trigEntryLanes = M.union (new^.trigEntryLanes) (old^.trigEntryLanes)}
              -- let fixedOld = old{_trigEntryLanes = new^.trigEntryLanes}
              -- return $ fixedOld /= new
              return $ old /= new
            )
        )
        & (/= mempty)
    in
      if diffTrackEntries || diffSeqs then
        Just $ oldTrack^.channel
      else
        Nothing
  )


postProcessModelUpdate :: SeqModel -> SeqModel -> (SeqModel, [ModeAction])
postProcessModelUpdate oldModel newModel =
  -- if (appInf^.appPlayerState.playing) == (newModel^._1.isRecording) then
  laneKills
  -- else
  --   killAll
  where
  -- killAll =(newModel, [0..127] & map (\i -> StopAllNotes $ fromIntegral i))
  laneKills = fromMaybe (oldModel, []) $ do
    oldTracks <- oldModel^?curArrangement
    newTracks <- newModel^?curArrangement
    let
      -- seqs = getUpdatedSeqs (oldModel^._1.triggerSeqGroups&toList) (newModel^._1.triggerSeqGroups&toList)
      -- lanes = seqs&map(\(old, new) -> (old^.trigEntryLanes, new^.trigEntryLanes))
      -- updatedLaneInds =
      --   lanes
      --   & map (\(old, new) -> getUpdatedLanes old new)
      --   & concat
      --   & map fst
      --

      updatedTrackEntryInds = getUpdatedTrackEntries (toList oldTracks) (toList newTracks) (oldModel^._1.triggerSeqGroups) (newModel^._1.triggerSeqGroups)

      actions =
        -- (updatedLaneInds++updatedTrackEntryInds)
        updatedTrackEntryInds
        & map (StopAllNotes . fromIntegral)

    return (newModel, actions)

appUpdate :: AppModeName -> AppInfo -> InputState -> SeqModel -> ControllerFunctions -> (ControllerUpdateFlag, Maybe AppModeName, SeqModel, [ModeAction])
appUpdate mode appInf input oldModel funcs =
  case switchMode of
    Just m -> (UpdateSingle, Just m, oldModel, [])
    Nothing ->
      modelUpdate appInf input oldModel funcs
      & fmap (\(modeState, flag, newMod, actions) ->
        let
          (outModel, postActions) = postProcessModelUpdate oldModel newMod
        in
          (flag, Nothing, outModel, postActions++actions)
      )
      & fromMaybe (NoUpdate, Nothing, oldModel, [])
  where
    modelUpdate = (appMode mode)^.update
    switchMode =
      if input&keyDown Shift then case input^.ev of
        -- InDown( RowPos(Side(7))  )-> Just triggerMode
        InDown(RowPos(Side x)) ->
          Just $ case x of
            0 -> ArrangeMode
            1 -> SeqEditMode
            2 -> LaneMode
            3 -> RootNoteMode
            4 -> NoteMapSelectMode
            _ -> mode
        _ -> Nothing
      else
        Nothing


appRender :: AppModeName -> InputState -> SeqModel -> ControllerFunctions -> [RenderEntry]
appRender mode input model funcs =
  if input&keyDown Shift then
    let maybeY = case mode of
          ArrangeMode -> Just 0
          SeqEditMode -> Just 1
          LaneMode -> Just 2
          RootNoteMode -> Just 3
          NoteMapSelectMode -> Just 4
          _ -> Nothing
    in maybeY&maybe [] (\y -> [ (RowPos $ Side y, StrongRed)])
  else []




handleFrame :: SeqModel -> AppInfo -> InputState -> ControllerFunctions -> (ControllerUpdateFlag, Maybe AppModeName, SeqModel, [ModeAction])
handleFrame m appInf input =
    appUpdate modeName appInf input m
    where
      modeName = m^._2.aMode

renderCurMode :: ControllerState -> AppInfo -> AppModel -> ControllerFunctions -> [RenderEntry]
renderCurMode contMod frameInfo app funcs =
  concat[
    appRender modeName input model funcs
    ,((appMode modeName)^.render) frameInfo input model funcs
  ]
  where
    modeName = contMod^.aMode
    model = (app&getCurSharedModel, contMod)
    input = contMod^.conInp






mainProcess :: ControllerState -> AppModel -> AppInfo -> ControllerFunctions -> (AppModel, ControllerState, ControllerUpdateFlag, [ModeAction])
mainProcess contMod oldApp appInf funcs =
  (newApp, newContMod, flag, action)
  where
    (flag, newModeNameMaybe, newSeqModel, action)
      = handleFrame (oldApp&getCurSharedModel, contMod) appInf (contMod^.conInp) funcs
    newSharedModel = newSeqModel^._1
    newContMod =
      (newSeqModel^._2)&(
        newModeNameMaybe&maybe
          id
          (\n -> aMode .~ n)
        )
    updateSeqs a =
      if newSharedModel == (oldApp&getCurSharedModel) then a
      else if a^.undoPushActive then
        a&pushUndoStep newSharedModel
      else
        a&curSharedModel .~ newSharedModel
    newApp =
      updateSeqs $ oldApp


pushUndoStep :: SharedModel -> AppModel -> AppModel
pushUndoStep newModel app =
  app&(seqModels %~ (\s -> newModel : take maxUndo (drop (app^.seqIndex) s)))
  .(seqIndex .~ 0)

processUndo :: ControllerState -> AppInfo -> AppModel -> ControllerFunctions -> (AppModel, ControllerState, ControllerUpdateFlag, [ModeAction])
processUndo contMod appInf app funcs =
  if inp&keyDown Shift then
    case inp^.ev of
      InDown(RowPos(Side 7)) -> (doCycle 1)
      InDown(RowPos(Side 6)) -> (doCycle (-1))
      _ -> mainProcess contMod app appInf funcs
  else
    mainProcess contMod app appInf funcs
  where
    inp = contMod^.conInp
    doCycle :: Int -> (AppModel, ControllerState, ControllerUpdateFlag, [ModeAction])
    doCycle step =
      let
        newApp = app&seqIndex %~ boundedAdd 0 ((app^.seqModels&length) - 1) step
      in
        (newApp, contMod, UpdateSingle, [])


processInput :: AppInfo -> ControllerState -> AppModel -> ControllerFunctions -> (AppModel, ControllerState, ControllerUpdateFlag, [ModeAction])
processInput appInf contMod appModel funcs=
  case inp^.ev of
    NoInput -> (appModel, contMod, NoUpdate, [])
    InDown _ -> run
    InUp _ -> run
    InNote _ -> run
  where
    inp = contMod^.conInp
    run = processUndo contMod appInf appModel funcs

processEvent :: TimeSpec -> AppInfo -> ControllerState -> AppModel -> MidiVoiceEvent -> ControllerFunctions -> (AppModel, ControllerState, ControllerUpdateFlag, [ModeAction])
processEvent curTime appInf contMod appModel inEvent funcs =
  processInput appInf newContMod appModel funcs
  where
    newInpState = contMod^.conInp&setInputEvent curTime e
    newContMod = (contMod&conInp.~newInpState)
    e = (funcs&runControllerProcessEvent) inEvent


getCurSharedModel :: AppModel -> SharedModel
getCurSharedModel app =
  app^?curSharedModel&fromMaybe (error "BUG: Selected seq model out of range.")


curSharedModel :: Traversal' AppModel SharedModel
curSharedModel f app =
  seqModels (ix (app^.seqIndex) f) app
