{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module AppMode where

import Sequencer
import Misc
import ControllerCommon
import Input
import Control.Lens
-- import AppModeName
import SeqConstants (sequenceDefaultLen)
import ControllerFunctions
import Data.List
import Data.Maybe
import Control.Monad
import Data.Monoid ((<>))
import Transport
import Player
import GHC.Word
import PlayerRunner

import Data.Foldable (toList)

data PlaybackAction =
  StartPlayback
  | StopPlayback
  | TogglePlayback
  deriving(Show, Eq)

data ModeAction =
  PlayNote Int Int
  | PlayNoteOneShot Int Int
  | StopPlayNote Int
  | StopAllNotes Word8 -- Channel
  | SetMaster
  | ClearMaster
  | DoTransport TransportAction
  | DoPlayback PlaybackAction
  | BPMChange Int
  deriving(Show, Eq)


data ControllerUpdateFlag =
  UpdateSingle
  | UpdateLastTouched
  | UpdateAll
  | NoUpdate
  deriving(Show)

data AppInfo = AppInfo {
  _isMaster :: Bool
  ,_appPlayerState :: PlayerState
  ,_appPlaybackState :: PlayerRunnerState
}
makeLenses ''AppInfo


type AppUpdateResult state = (
    state
    ,ControllerUpdateFlag -- Strategy for applying updater
    ,SeqModel
    ,[ModeAction] -- Actions created this frame to be executed
  )


-- newtype AppModeUpdate = AppModeUpdate {
type AppModeUpdate' state =
  -- getUpdate:: (
      AppInfo -- Frame-specific information
      -> InputState
      -> SeqModel
      -> ControllerFunctions -- Record of functions implementing functionality for a control surface
      -> Maybe (AppUpdateResult state)
    -- )
-- }

type AppModeUpdate = AppModeUpdate' ()

type RenderEntry = (Cell, Color)

type AppModeRender= AppInfo -> InputState -> SeqModel -> ControllerFunctions -> [RenderEntry]


data AppMode' state = AppMode {
  _renderPlayPosition :: AppModeRender
  ,_update :: AppModeUpdate' state
  ,_render :: AppModeRender
}

type AppMode = AppMode' ()

makeLenses ''AppMode'




instance Monoid (AppMode' state) where
  mempty = nopMode
  mconcat = metaMode
  mappend a b = mconcat [a,b]



type SimpleUpdate = AppInfo -> InputState -> SeqModel -> ControllerFunctions -> Maybe SeqModel
type ActionUpdate = AppInfo -> InputState -> SeqModel -> ControllerFunctions -> Maybe [ModeAction]

simpleUpdate :: SimpleUpdate -> AppModeUpdate
simpleUpdate updater appInf inp m funcs =
  let mbNewMod = updater appInf inp m funcs
  in
    mbNewMod&fmap (\newMod -> ((), UpdateSingle, newMod, []))

actionUpdate :: ActionUpdate -> AppModeUpdate
actionUpdate updater appInf inp m funcs =
  let mbNewMod = updater appInf inp m funcs
  in
    mbNewMod&fmap (\acts -> ((), UpdateSingle, m, acts))


nopModeFun _ _ _ _ = mzero


nopMode = AppMode {
  _update = nopModeFun
  ,_render = nopModeFun
  ,_renderPlayPosition = nopModeFun
}


simpleAppMode upd rend =
  nopMode {_update = simpleUpdate upd, _render=rend}



clearGridCellRender :: ControllerFunctions -> Int -> RenderEntry
clearGridCellRender funcs ind =
  let cell = (funcs&gridFromIndex) ind
  in  (cell, NoCol)



clearRender :: ControllerFunctions -> [RenderEntry]
clearRender funcs =
  concat[
    [0..((funcs&viewWidth) * (funcs&viewHeight) -1)] & map (funcs&clearGridCellRender )
    ,[RowPos(Top i) | i<-[0..(funcs&rowPosTopCount )-1] ]&(
      map (\x ->  (x, NoCol))
    )
    ,[RowPos(Side i) | i<-[0..(funcs&rowPosSideCount)-1] ]&(
      map (\x ->  (x, NoCol))
    )
  ]

clearBottomRender :: ControllerFunctions -> [RenderEntry]
clearBottomRender funcs =
  [(RowPos(Top x), NoCol) | x <-[0..(funcs&viewWidth)-1]]


clearSideRender :: ControllerFunctions -> [RenderEntry]
clearSideRender funcs =
  [(RowPos(Side x), NoCol) | x <-[0..(funcs&viewHeight)-1]]


rootNoteEditOffset :: Int -> SeqModel -> ControllerFunctions -> Int
rootNoteEditOffset trackEntryIndex m funcs =
  trackEntryIndex*sequenceDefaultLen+(m^._2.rootSeqEditPage)*(funcs&viewWidth )


curRootNoteEditOffset :: SeqModel -> ControllerFunctions -> Maybe Int
curRootNoteEditOffset m funcs =
  Just $ rootNoteEditOffset (m^._2.selectedTrackPos) m funcs


indexSelectRender funcs selCol maxInd selInd =
  if selInd >= maxInd then []
  else [ ((funcs&gridFromIndex ) selInd,selCol )]


indexSelectRenderWithBg funcs bgCol selCol maxInd selInd =
  [0..maxInd-1]&map (\ind ->
    let col = if ind == selInd then selCol else bgCol
    in  ((funcs&gridFromIndex ) ind, col)
  )


noteMapSelectMode :: AppMode
noteMapSelectMode = nopMode

{-
   noteMapSelectMode :: AppMode
   noteMapSelectMode = mempty {
     _update = simpleUpdate $ \_ input m funcs ->
       case input^.ev of
         InDown cell@(GridXY _ _) ->
           let ind = ( funcs&indexFromCell ) cell
           in if ind < ( m^._1.rootNoteSeqs&length ) then
             m
             & curTrack %~  (rootSeqID.~ind)
             & _2.aMode .~ SeqEditMode
             & Just
           else
             Nothing
         _ -> Nothing
   
     ,_render = \_ _ m funcs ->
       m^?curTrack&maybe [] (\track ->
         indexSelectRender funcs StrongAmber (m^._1.rootNoteSeqs&length) (track^.rootSeqID)
       )
   }
-}

noteSelectCommonRender :: ControllerFunctions -> Color -> Int -> SeqModel -> NoteEntry -> Int -> (Color, Cell)
noteSelectCommonRender funcs baseCol offset model note ind =
  (col, GridXY ind (noteRealY funcs model y))
  where
    y = offset - ( model&trackNoteToNum note )
    col
      | y<0 || y>(funcs&viewHeight )-1 = StrongAmber
      | otherwise = baseCol


noteRealY :: ControllerFunctions -> SeqModel -> Int -> Int
noteRealY funcs model y
  | y < 0 = 0
  | y > (funcs&viewHeight )-1 = (funcs&viewHeight )-1
  | otherwise = y



gridRender :: ControllerFunctions -> (Int -> Maybe Color) -> [Maybe a] -> [RenderEntry]
gridRender funcs colFun entries =
  entries&mapInd (\trigger ind ->
    trigger >>= (const$
      let cell = (funcs&gridFromIndex ) ind
      in
        colFun ind & fmap(\col ->
            (cell, col)
          )
    )
  )
  &catMaybes




runOtherModeUpdate ::
  ControllerFunctions
  -> AppModeUpdate
  -> AppInfo
  -> InputState
  -> SeqModel
  -> Maybe (SeqModel, [ModeAction])
runOtherModeUpdate funcs upd appInf inp m =
  let res =  upd appInf inp m funcs
  in
    res&fmap(\((), _, newMod, actions)-> (newMod, actions))



differ :: Eq b => Lens' a b -> a -> a -> (a -> a)
differ lns base new =
  if (base^.lns) /= newV then
    (lns .~ newV)
  else
    id
  where
    newV = new^.lns


contModDiff :: ControllerState -> ControllerState -> ControllerState -> ControllerState
contModDiff base new model =
    [
      differ rootSeqEditPage
      ,differ selectedTriggerEntry
      ,differ rootCenterNote
      ,differ centerEditNote
      ,differ curTrackIndex
      ,differ curLaneInd
      ,differ lastSelTrigger
      ,differ laneViewOffset
      ,differ arrViewOffset
      -- ,aMode
      -- ,conInp
      ,differ selectedTrackPos
    ]
    \> foldl' (\m f -> f base new  m) model


contOptionToggler :: (Bounded a, Eq a) => Cell -> Lens' ControllerState a -> AppMode
contOptionToggler key lns =
  togglerMode (_2.lns) key


contOptionTogglerWith :: (Bounded a, Eq a) => Cell -> Lens' ControllerState a -> AppMode -> AppMode  -> AppMode
contOptionTogglerWith key lns subMode defaultMode =
  togglerModeWith subMode defaultMode key (_2.lns)



metaMode :: [AppMode' state] -> AppMode' state
metaMode modes = AppMode {
  _renderPlayPosition = metaRender (modes&map _renderPlayPosition)
  ,_update = metaUpdate (modes&map _update)
  ,_render = metaRender (modes&map _render)
}


metaRender :: Foldable t => t (t1 -> t2 -> t3 -> t4 -> [b]) -> t1 -> t2 -> t3 -> t4 -> [b]
metaRender renders appInf inp model funcs =
  renders
  & concatMap (\rend -> rend appInf inp model funcs)


-- TODO: Can we somhow not use toList here?
metaUpdate :: Foldable f => f (t -> t1 -> t2 -> t3 -> Maybe a) -> t -> t1 -> t2 -> t3 -> Maybe a
metaUpdate updates appInf inp model funcs =
  updates
  & toList
  & mapMaybe (\upd -> upd appInf inp model funcs)
  & listToMaybe


runModeIf :: (InputState -> SeqModel -> Bool) -> (AppMode -> AppMode) -> AppMode -> AppMode
runModeIf = runModeIf' id


runModeIf' ::
  (InputState -> InputState)
  -> (InputState -> SeqModel -> Bool)
  -> (AppMode -> AppMode)
  -> AppMode
  -> AppMode
runModeIf' inpTransformer predicate mode defaultMode = AppMode {
    _renderPlayPosition = run _renderPlayPosition
    ,_update = run _update
    ,_render = run _render
  }
  where
    getMode inp m =
      if predicate inp m then mode defaultMode
      else
        defaultMode

    run fun appInf inp model =
      let inpTrans = if predicate inp model then inpTransformer else id
      in ((getMode inp model)&fun) appInf (inpTrans inp) model


togglerMode' :: Eq a => Cell -> Lens' SeqModel a -> a -> a -> AppMode
togglerMode' key lns v1 v2 = AppMode {
  _renderPlayPosition = nopModeFun

  ,_update = \appInf inp m funcs->
    case inp^.ev of
      InDown cell ->
        if cell == key then
          Just $ ((), UpdateSingle, m&lns %~ (\v -> if v==v1 then v2 else v1), [])
        else
          Nothing
      _ -> Nothing

  ,_render = \appInf inp m funcs ->
    if m^.lns == v1 then
      [(key, StrongRed)]
    else
      [(key, NoCol)]

}


togglerMode :: (Eq a, Bounded a) => Lens' SeqModel a -> Cell -> AppMode
togglerMode lns key =
  togglerMode' key lns maxBound minBound


togglerModeWith' :: Eq b => AppMode -> AppMode -> Cell -> Lens' SeqModel b -> b -> b -> AppMode
togglerModeWith' subMode defaultMode key lns v1 v2 =
  -- runModeIf (\i m -> m^.lns == v1 || (i&noOtherKeysDown)) (const $
      let toggle = togglerMode' key lns v1 v2
      in
        runModeIf (\_ m -> m^.lns == v1) (const $ subMode <> toggle ) (toggle <> defaultMode )
    -- ) defaultMode


togglerModeWith :: (Bounded a, Eq a) => AppMode -> AppMode -> Cell -> Lens' SeqModel a -> AppMode
togglerModeWith subMode defaultMode key lns =
  togglerModeWith' subMode defaultMode key lns maxBound minBound


runModeIfDown' :: [Cell] -> AppMode -> AppMode
runModeIfDown' keys m  =
  runModeIfDown keys m nopMode


runModeIfDown :: [Cell] -> AppMode -> AppMode -> AppMode
runModeIfDown keys m  =
  runModeIf' (filterKeys keys) (\inp _ -> inp&keysDown keys) (const $ m)


runModeIfDownOnly :: [Cell] -> AppMode -> AppMode -> AppMode
runModeIfDownOnly keys m =
  runModeIf' (filterKeys keys ) (\inp _ -> (inp&getDownKeys) ==  keys) (const $ m)


runUpdateIfDown :: [Cell] -> AppMode -> AppMode
runUpdateIfDown keys mode =
  mode{_update=nopModeFun}
  <> runModeIfDown' keys nopMode{_update = mode^.update}

runModeIfHeld :: [Cell] -> AppMode -> AppMode -> AppMode
runModeIfHeld keys m  =
  runModeIf' (filterKeys keys) (\inp _ -> inp&keysHeld keys) (const $ m)



runModes :: [(AppMode -> AppMode)] -> AppMode -> AppMode
runModes modes defaultMode =
  modes
  & reverse
  & foldl' (\lastMode mode ->
      mode lastMode
    ) defaultMode


overrideUpdateFlagIf :: (AppInfo -> InputState -> SeqModel -> Bool) -> ControllerUpdateFlag -> AppMode -> AppMode
overrideUpdateFlagIf predicate flag mode =
  mode{
    _update = \appInf inp m funcs ->
      (mode^.update) appInf inp m funcs
      & fmap(\(_, resFlag,resM,resActs) ->
          if predicate appInf inp m then
            ((), flag, resM, resActs)
          else
            ((), resFlag, resM, resActs)
        )
  }

overrideUpdateFlag :: ControllerUpdateFlag -> AppMode -> AppMode
overrideUpdateFlag =
  overrideUpdateFlagIf (\_ _ _ -> True)



offsetterMode :: Lens' ControllerState (Int,Int) -> AppMode -> AppMode
offsetterMode offsetLens subMode = AppMode{
    _render = wrapRender (subMode^.render)
    ,_renderPlayPosition = wrapRender (subMode^.renderPlayPosition)
    ,_update = \appInf inp m funcs->
      let
        offX = (m^._2)^.offsetLens._1
        offY = (m^._2)^.offsetLens._2

        newInp = offsetInput offX offY inp
      in
        (subMode^.update) appInf newInp m funcs
  }
  where
    wrapRender :: AppModeRender -> AppInfo -> InputState -> SeqModel -> ControllerFunctions -> [RenderEntry]
    wrapRender rend appInf inp m funcs =
      let
        offX = (m^._2)^.offsetLens._1
        offY = (m^._2)^.offsetLens._2

        newInp = offsetInput offX offY inp
      in
        (rend appInf newInp m funcs)
        & map(_1 %~ offsetCell (-offX) (-offY))


recToggleMode = togglerMode (_1.isRecording)



-- data AppModeRunner a = 
--
-- runModes :: Monoid a => AppModeRunner a -> AppMode
-- runModes _ = nopMode
--
--
-- instance MonadPlus AppModeRunner where
--   mzero = nopMode
--   [] `mplus` nopMode = nopMode
--   -- Just x  `mplus` Nothing = Just x  -- 1 solution  + 0 solutions = 1 solution
--   -- Nothing `mplus` Just x  = Just x  -- 0 solutions + 1 solution  = 1 solution
--   -- Just x  `mplus` Just y  = Just x  -- 1 solution  + 1 solution  = 2 solutions,
--   --                                 -- but Maybe can only have up to one solution,
--   --                               -- so we disregard the second one.
--



-- mrunModeIf:: (InputState -> SeqModel -> Bool) -> AppMode -> Maybe (AppUpdateResult, [RenderEntry])
-- mrunModeIf predicate mode defaultMode = AppMode {
--     renderPlayPosition = run renderPlayPosition
--     ,update = run update
--     ,render = run render
--   }
--   where
--     getMode inp m =
--       if predicate inp m then mode
--       else
--         defaultMode
--
--     run fun appInf inp model =
--       ((getMode inp model)&fun) appInf inp model









-- -- TODO: Can we somehow not use toList here?
-- runModes :: (Functor t, Foldable t) => t AppMode -> AppMode
-- runModes modes = AppMode {
--   renderPlayPosition = \frameInf inp model funcs ->
--     doRender renderPlayPosition appInf frameInf inp model funcs
--
--   ,update =
--     metaUpdate (modes & fmap update)
--
--   ,render = \appInf inp model funcs ->
--     doRender render appInf inp model funcs
-- }
--   where
--     doRender rendFun appInf frameInf inp model funcs=
--       modes
--       & toList
--       & mapMaybe (\mode ->
--           (mode&update) appInf inp model funcs
--           & fmap (const $ mode)
--         )
--       & listToMaybe
--       & maybe [] (\mode -> (mode&rendFun) frameInf inp model funcs)
--


playbackToggler :: Cell -> AppMode' ()
playbackToggler key = nopMode {
  _update = \appInf inp m funcs ->
    case inp^.ev of
      InDown k ->
        if k == key then
          Just ((), UpdateSingle, m, [DoPlayback TogglePlayback])
        else Nothing
      _ -> Nothing

  ,_render = \appInf _ m funcs ->
    if (appInf^.appPlaybackState.playing) then
      [(key, StrongRed)]
    else []
}


bpmChanger :: Cell -> Cell -> AppMode' ()
bpmChanger keyDec keyInc = nopMode {
  _update = \appInf inp m funcs ->
    case inp^.ev of
      InDown k ->
        if k == keyDec then
          Just ((), UpdateSingle, m, [BPMChange $ -1])
        else if k == keyInc then
          Just ((), UpdateSingle, m, [BPMChange 1])
        else Nothing
      _ -> Nothing

}
