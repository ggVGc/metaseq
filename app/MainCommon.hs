{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module MainCommon where


import Control.Lens
import Player
import Misc
import Sequencer
import Application
import AppMode
import Control.Concurrent
import ZMidi.Core
import SequenceRender
import Modes
import Data.Maybe
import Control.Monad
import ControllerFunctions
import Data.List
-- import Network.Simple.TCP
import StateFileIO
-- import NetworkPlay
import Input
import System.Clock
import Control.Arrow((>>>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Recorder
import ModeActions
import Transport
import PlayerRunner
import SeqConstants


playLoopDelay = 4000
noteInLoopDelay = 4000
sessionRenderDelay = 20000
controllerSessionDelay = 50000
songRenderDelay = 100000
stateWriteDelay = 5000000

type ControllerInput state =  state -> IO (Maybe ([MidiVoiceEvent], state))
type ControllerRender =  [MidiEvent] -> IO ()
type NoteInputGetter = Int -> IO [MidiVoiceEvent]

type ControllerUpdateState = (ControllerUpdateFlag, (ControllerState -> ControllerState), ControllerState)

data ControllerSessionConfig state = ControllerSessionConfig {
    _sInp :: ControllerInput state
    , _sRender :: ControllerRender
    , _sFuncs :: ControllerFunctions
    , _sState :: state
}

type SessionRenderInfo = (Maybe [RenderEntry], Maybe [RenderEntry])


data ControllerSession state = ControllerSession {
    _sConf :: ControllerSessionConfig state
    , _sUpdVar :: MVar ControllerUpdateState
    , _sId :: Int
  }



makeLenses ''ControllerSessionConfig
makeLenses ''ControllerSession

instance Eq (ControllerSession a) where
  x == y = (x^.sId) == (y^.sId)


songRenderLoop :: MVar AppModel -> MVar(Maybe PlayerEvents) -> MVar Int -> IO ()
songRenderLoop appModelVar playerEventsVar renderTriggerVar = do
    -- (sock, addr) <- connectSock"127.0.0.1" "8888"
    -- putStrLn $ "Connection established to " ++ show addr
  -- print "Waiting for trigger"
  _ <- takeMVar renderTriggerVar
  print "Rendering song"
  -- print "Reeading shared model"
  appModel <- readMVar appModelVar
  let sharedModel = appModel&getCurSharedModel
  -- print "Rendering"
  let song = renderSeqModel sharedModel
  -- sock&sendReset
  -- mbSongEntries&mapM_(\songEnts ->
  --   songEnts&mapM_(\trackEnts ->
  --         sendTrackEvents sock trackEnts
  --       )
  --     )
  --
  let evs = playerTransformEvents song
  -- print "Swapping player events"
  _ <-  swapMVar playerEventsVar (Just evs)

  _ <- threadDelay songRenderDelay
  songRenderLoop appModelVar playerEventsVar renderTriggerVar


makeAppInfo ::
  MVar PlayerState
  -> MVar PlayerRunnerState
  -> MVar (Maybe (ControllerSession b), [ControllerSession b])
  -> ControllerSession b
  -> IO AppInfo
makeAppInfo playerStateVar playerRunnerVar lastUpdatedSessionVar session = do
  playerState <- readMVar playerStateVar
  (mbMasterSess, _) <- readMVar lastUpdatedSessionVar
  runnerState <- readMVar playerRunnerVar
  return $ AppInfo {
    _isMaster = (mbMasterSess&maybe False (\s -> s == session))
    ,_appPlayerState= playerState
    ,_appPlaybackState = runnerState
  }


type TransportActionRunner = TransportAction -> IO ()

genericAppLoop ::
  String
  -> NoteInputGetter
  -> IO [MidiEvent]
  -> ([MidiVoiceEvent] -> IO t2)
  -> (AppModel -> IO t1)
  -> AppModel
  -> TransportActionRunner
  -> [(ControllerSessionConfig b)]
  -> IO ()
genericAppLoop stateDir noteInGetter getSyncEvents playNotes frameFunc appModel transportRunner inSessions = do
  curTime <- getTime Monotonic
  tickDeltaVar <- newMVar (0, curTime)
  appModelVar <- newMVar appModel
  let initialSongEvents = renderSeqModel $ appModel&getCurSharedModel
  playerEventsVar <- newMVar Nothing :: IO(MVar (Maybe PlayerEvents))
  playerStateVar <- newMVar (playerSetEvents (playerTransformEvents initialSongEvents) defaultPlayerState)
  sessions <- inSessions&mapInd(,)&mapM(uncurry makeInitialSession)
  lastUpdatedSessionVar <- newMVar(sessions&listToMaybe&fmap fst, sessions&drop 1&map fst)
  playRunnerVar <- newMVar defaultPlayerRunnerState
  let getAppInfo = makeAppInfo playerStateVar playRunnerVar lastUpdatedSessionVar
  let onBeatChange = onNewPlayerPos getAppInfo appModelVar sessions

  -- renderModelVar <- newMVar (appModel&getCurSharedModel)
  renderTriggerVar <- newEmptyMVar :: IO(MVar Int)
  let renderSong = do
        -- print "want to render"
        -- _ <- swapMVar renderModelVar newSharedModel
        -- print "swapped"

        -- curAppModel <- readMVar appModelVar
        _ <- tryPutMVar renderTriggerVar 1
        -- print "triggered"
        return ()
  _ <- forkIO $ songRenderLoop appModelVar playerEventsVar renderTriggerVar >> return ()

  recorderVar <- newMVar defaultRecorderState

  -- _ <- forkIO $ syncPlay transportRunner getSyncEvents playNotes playerStateVar playerEventsVar onBeatChange tickDeltaVar
  let bpmGetter = do
        curAppModel <- readMVar appModelVar
        return $
          (curAppModel&getCurSharedModel)^.bpm
  _ <- forkIO $ playLoop playerStateVar playRunnerVar playNotes transportRunner playerEventsVar onBeatChange bpmGetter 0
  lastSavedModelVar <- newMVar defaultSharedModel
  _ <- forkIO $ stateWriteLoop stateDir appModelVar lastSavedModelVar

  let handleActions = actionHandler transportRunner lastUpdatedSessionVar recorderVar playNotes appModelVar playerStateVar playRunnerVar renderSong tickDeltaVar
  _ <- forkIO $ noteInLoop noteInGetter renderSong appModelVar lastUpdatedSessionVar (renderSessions id sessions getAppInfo) handleActions getAppInfo playNotes recorderVar playerStateVar
  let onUpdate = onSessionUpdate renderSong frameFunc lastUpdatedSessionVar sessions getAppInfo
  sessions&mapM_ (\(s, rendVar)-> do
      _ <- forkIO $ processControllerSession (onUpdate s) appModelVar (s, rendVar) handleActions getAppInfo renderSong recorderVar playerStateVar
      forkIO $ sessionRenderLoop (s^.sConf.sFuncs, s^.sConf.sRender, (clearRender (s^.sConf.sFuncs)), rendVar) [] ([], [])
    )
  renderSessions id sessions getAppInfo appModel



makeInitialSession :: ControllerSessionConfig a -> Int -> IO (ControllerSession a, MVar SessionRenderInfo)
makeInitialSession conf ind = do
  rendVar <- newMVar (Nothing, Nothing)
  contModVar <-  newMVar (UpdateSingle, id, defaultControllerState)
  return $ (ControllerSession conf contModVar ind, rendVar)

renderSessions ::
  (ControllerState -> ControllerState)
  -> [(ControllerSession b, MVar SessionRenderInfo)]
  -> (ControllerSession b -> IO AppInfo)
  -> AppModel
  -> IO ()
renderSessions contModProcessor sessions getAppInfo appModel =
  sessions&mapM_ (\(sess, renderVar) -> do
      (_, _, contMod) <- readMVar (sess^.sUpdVar)
      appInfo <- getAppInfo sess
      let
        evs = renderCurMode (contModProcessor contMod ) appInfo appModel (sess^.sConf.sFuncs)
      putMVar renderVar (Just evs, Nothing)
    )



noteInLoop ::
  NoteInputGetter
  -> IO ()
  -> MVar AppModel
  -> MVar (Maybe (ControllerSession a), [ControllerSession a])
  -> (AppModel -> IO())
  -> (ControllerSession a -> [ModeAction] -> IO ())
  -> (ControllerSession a -> IO AppInfo)
  -> ([MidiVoiceEvent] -> IO b)
  -> MVar RecorderState
  -> MVar PlayerState
  -> IO ()
noteInLoop noteInputGetter renderSong appModelVar lastUpdatedSessionVar renderAllSessions handleActions getAppInfo playNotes recorderVar playerStateVar = do
  (_, lastSessions) <- readMVar lastUpdatedSessionVar
  (lastSessions^?ix 0)&mapM_(\session -> do
      let contModVar = session^.sUpdVar
      (_, _, contMod) <- readMVar contModVar
      oldApp <- readMVar appModelVar
      let curModel = (oldApp&getCurSharedModel, contMod)
      -- let noteInputGetter = session^.sConf.sNoteInpGetter
      evs <- noteInputGetter (curModel^?curTrack.channel&fromMaybe 0)

      let inpNotes =
            evs&mapMaybe(\e -> case e of
                NoteOn _ k v ->  Just $ NoteDown ((fromIntegral k), (fromIntegral v))
                NoteOff _ k _ ->  Just $ NoteUp (fromIntegral k)
                _ -> Nothing
              )

      case inpNotes of
        [] -> return ()
        firstNote:restNotes ->
          if (curModel^._1)^.isRecording then do
          let
            (noteDowns, noteUps) =
              inpNotes&ffoldl ([],[]) (\(downs, ups) n->
                case n of
                  NoteDown x-> (x:downs, ups)
                  NoteUp x -> (downs, x:ups)
              )
          beginRecNotes noteDowns recorderVar playerStateVar curModel
          stopRecNotes noteUps playerStateVar appModelVar recorderVar contMod
          -- case firstNote of
          --   NoteDown (n, v) ->
          --   NoteUp n ->
          else do
            print inpNotes
            appInfo <- getAppInfo session
            curTime <- getTime Monotonic
            let
              newInp = contMod^.conInp & (
                    setInputEvent curTime $ InNote (firstNote:|restNotes)
                  )

              funcs = session^.sConf.sFuncs
              (newApp, newContMod, flag, actions) = processInput appInfo (contMod&conInp.~newInp) oldApp funcs

            _ <- swapMVar contModVar (flag, contModDiff contMod newContMod, newContMod)
            _ <- swapMVar appModelVar newApp
            let newSharedModel = newApp&getCurSharedModel
            _ <- handleActions  session actions
            when ((not $ newSharedModel^.isRecording) && ( curModel /= (newSharedModel, newContMod))) (do
                renderAllSessions newApp
                renderSong
                return ()
              )
            return ()
      )
  threadDelay noteInLoopDelay
  noteInLoop noteInputGetter renderSong appModelVar lastUpdatedSessionVar renderAllSessions handleActions getAppInfo playNotes recorderVar playerStateVar



actionHandler ::
  TransportActionRunner
  -> MVar (Maybe (ControllerSession b), [ControllerSession b])
  -> MVar RecorderState
  -> ([MidiVoiceEvent] -> IO a)
  -> MVar AppModel
  -> MVar PlayerState
  -> MVar PlayerRunnerState
  -> IO ()
  -> MVar (TimeSpec, TimeSpec)
  -> ControllerSession b
  -> [ModeAction]
  -> IO ()
actionHandler transportRunner lastUpdatedSessionVar recorderVar  playNotes appModelVar playerStateVar playRunnerVar songRenderer _{- tickDeltaVar -} updatedSession actions = do
  let contModVar = updatedSession^.sUpdVar
  (_, _, contMod) <- readMVar contModVar
  appModel <- readMVar appModelVar
  actions&mapM_ (handleAction (appModel&getCurSharedModel, contMod))
  where
    handleAction :: SeqModel -> ModeAction -> IO()
    handleAction curModel a = case a of
      PlayNote n v->
        curModel&playNote (fromIntegral n) (fromIntegral v) playNotes

      PlayNoteOneShot n v-> do
        _ <- curModel&playNote (fromIntegral n) (fromIntegral v) playNotes
        _ <- forkIO $ do
          _ <- threadDelay 100000
          curModel&stopPlayNote (fromIntegral n) playNotes
        return ()

      StopPlayNote n -> curModel&stopPlayNote (fromIntegral n) playNotes
      StopAllNotes chan -> do
        print $ "Killing notes for chan: "++show chan
        _ <- playNotes $ stopAllChannelNotes chan
        return ()
      SetMaster ->
        modifyMVar_ lastUpdatedSessionVar (pure . (_1 .~ Just updatedSession))
      ClearMaster ->
        modifyMVar_ lastUpdatedSessionVar (pure . (_1 .~ Nothing))
      BPMChange change ->
          modifyMVar_ appModelVar (pure . (curSharedModel.bpm +~ change))
      DoTransport transAction -> case transAction of
        SetSongPos pos -> do
          transportRunner transAction
          modifyMVar_ playerStateVar (pure . playerSetPos pos)
        SetLoopBegin start ->
          modifyMVar_ playerStateVar (pure . (setLoopStart start))
        SetLoopEnd end ->
          modifyMVar_ playerStateVar (pure . (setLoopEnd end))
        StartLoop ->
          modifyMVar_ playerStateVar (\playerState -> pure $
                playerState{looping=True}
              )
        StopLoop ->
          modifyMVar_ playerStateVar (\playerState -> pure $
                playerState{looping=False}
              )
        x -> transportRunner x

      DoPlayback act ->
        let
          changePlayState newPlaying = do
            _ <- playNotes $ stopAllNotes
            modifyMVar_ playerStateVar (\player -> pure $
                  player&playerReturnToLoopStart
                )
            modifyMVar_ playRunnerVar (\runner -> pure $
                  runner{_playing = newPlaying, _skipNextStep = newPlaying}
                )
        in case act of
          StartPlayback -> changePlayState True
          StopPlayback -> changePlayState False
          TogglePlayback -> do
            _ <- playNotes $ stopAllNotes
            runner <- readMVar playRunnerVar
            (handleAction curModel) . DoPlayback $
              if (runner^.playing) then
                StopPlayback
              else
                StartPlayback



processControllerSession ::
  (AppModel -> SharedModel -> IO a)
  -> MVar AppModel
  -> (ControllerSession b, MVar SessionRenderInfo)
  -> (ControllerSession b -> [ModeAction] -> IO ())
  -> (ControllerSession b -> IO AppInfo)
  -> IO()
  -> MVar RecorderState
  -> MVar PlayerState
  -> IO d
processControllerSession onAppUpdate appModelVar (contSession, renderVar) handleActions getAppInfo renderSong recorderVar playerStateVar = do
  let
    contModVar = contSession^.sUpdVar
    oldState = contSession^.sConf.sState
    funcs = contSession^.sConf.sFuncs

  mayInpRes <- (contSession^.sConf.sInp) oldState
  (_, _, contMod) <- readMVar contModVar
  mayInpRes&maybe
    (threadDelay controllerSessionDelay >> processControllerSession onAppUpdate appModelVar (contSession, renderVar) handleActions getAppInfo renderSong recorderVar playerStateVar)
    (\(inpEvents, newState) -> do
        oldAppModel <- takeMVar appModelVar
        appInf <- getAppInfo contSession
        curTime <- getTime Monotonic
        let
          oldSharedModel = oldAppModel&getCurSharedModel
          (newApp, newContMod, flag, actions) = foldl'  (\(appMod, con, _, _) e ->
                let (a, b, f, acts) = processEvent curTime appInf con appMod e funcs
                in (a,b, f, acts)
            ) (oldAppModel&undoPushActive .~ (not $ oldSharedModel^.isRecording), contMod, UpdateSingle, []) inpEvents

        let
          newSharedModel = newApp&getCurSharedModel

        _ <- putMVar appModelVar newApp
        when(newContMod /= contMod || newSharedModel /= oldSharedModel)(do
          _ <- swapMVar contModVar (flag, contModDiff contMod newContMod, newContMod)
          _ <- onAppUpdate newApp oldSharedModel
          _ <- handleActions  contSession actions

          when((oldSharedModel^.isRecording) &&  (not $ newSharedModel^.isRecording)) (do
            stopAllRec playerStateVar appModelVar recorderVar contMod
            renderSong
            modifyMVar_ appModelVar (\app ->
                pure (
                  if newSharedModel /= oldSharedModel then
                    pushUndoStep (getCurSharedModel app) app
                  else
                    app
                )
              )
            )
          )

        processControllerSession onAppUpdate appModelVar (contSession&sConf.sState .~ newState, renderVar) handleActions getAppInfo renderSong recorderVar playerStateVar
    )


stateWriteLoop ::
  String
  -> MVar AppModel
  -> MVar SharedModel
  -> IO ()
stateWriteLoop stateDir appModelVar lastSavedModelVar = do
  threadDelay stateWriteDelay
  appModel <- readMVar appModelVar
  lastMod <- readMVar lastSavedModelVar
  let sharedMod = appModel&getCurSharedModel
  unless (sharedMod^.isRecording || (sharedMod == lastMod))(do
      print "Writing state"
      _ <- swapMVar lastSavedModelVar sharedMod
      writeStateFile sharedMod stateDir
    )
  stateWriteLoop stateDir appModelVar lastSavedModelVar


onSessionUpdate ::
  IO()
  -> (AppModel -> IO a)
  -> MVar (Maybe (ControllerSession b), [ControllerSession b])
  -> [(ControllerSession b, MVar SessionRenderInfo)]
  -> (ControllerSession b -> IO AppInfo)
  -> ControllerSession b
  -> AppModel
  -> SharedModel
  -> IO ()
onSessionUpdate songRenderer frameFunc lastUpdatedSessionVar allSessions getAppInfo updatedSession newAppModel oldSharedModel = do
  when(oldSharedModel /=  (newAppModel&getCurSharedModel))
      songRenderer

  _ <- frameFunc newAppModel
  (updateFlag, curUpdater, _) <- readMVar (updatedSession^.sUpdVar)
  (mbMasterSess, lastSessions) <- readMVar lastUpdatedSessionVar
  modifyMVar_ lastUpdatedSessionVar (pure . (_2 %~(
        ((:) updatedSession)
        >>> nub
        >>> take (length allSessions)
      ))
    )
  let
    nonMasterSessions = lastSessions&filter(\s -> mbMasterSess&maybe True (/= s))
    lastOtherTouchedSession = nonMasterSessions^?ix 0
    isMasterUpdate = mbMasterSess&maybe False (== updatedSession )
    newLastSession = lastOtherTouchedSession&maybe False (/= updatedSession)

  case updateFlag of
    UpdateSingle -> do
      unless isMasterUpdate (
          if newLastSession then
            replicateContMod (Just updatedSession) mbMasterSess
          else
            applyUpdater (Just updatedSession) mbMasterSess
        )
      -- let sharedModel = newAppModel\>getCurSharedModel
      -- lastOtherTouchedSession&mapM_ (shareIfSameSeq sharedModel allSessions)
      return ()

    UpdateLastTouched ->
      when isMasterUpdate (
          applyUpdater mbMasterSess lastOtherTouchedSession
        )

    _ ->
      when newLastSession (do
          replicateContMod (Just updatedSession) mbMasterSess
          return ()
        )

  renderSessions (\contMod ->
    case updateFlag of
      UpdateAll -> curUpdater contMod
      _ -> contMod
    ) allSessions getAppInfo  newAppModel
  return ()
  where
    applyUpdater :: Maybe(ControllerSession b) ->  Maybe(ControllerSession b) -> IO ()
    applyUpdater mbFrom mbTo =
      mbFrom&mapM_(\fromSes -> mbTo&mapM_(\toSes -> do
          (_,updater,_) <- readMVar (fromSes^.sUpdVar)
          (a, b, contMod) <- takeMVar (toSes^.sUpdVar)
          let newContMod = updater contMod
          putMVar (toSes^.sUpdVar) (a,b,newContMod)
        ))


replicateContMod :: Maybe(ControllerSession b) ->  Maybe(ControllerSession c) -> IO ()
replicateContMod mbFrom mbTo =
  mbFrom&mapM_(\fromSes -> mbTo&mapM_(\toSes -> do
      (_,_,fromContMod) <- readMVar (fromSes^.sUpdVar)
      (a, b, oldContMod) <- takeMVar (toSes^.sUpdVar)
      let
        newContMod = fromContMod&(
                  (aMode .~ (oldContMod^.aMode))
                  .(conInp .~ (oldContMod^.conInp))
                )
      putMVar (toSes^.sUpdVar) (a,b,newContMod)
    ))


shareIfSameSeq :: SharedModel -> [(ControllerSession b, c)] -> ControllerSession b ->  IO ()
shareIfSameSeq sharedModel allSessions lastSession =
  allSessions&mapM_(\(s,_) -> do
      (_,_,contMod) <- readMVar (s^.sUpdVar)
      (_,_,oldContMod) <- readMVar (lastSession^.sUpdVar)
      let
        m = (sharedModel, contMod)
        oldM = (sharedModel, oldContMod)
      when(
          (m^?curTriggerVariation) == (oldM^?curTriggerVariation)
        ) (
          replicateContMod (Just lastSession) (Just s)
        )
    )


onNewPlayerPos ::
  (ControllerSession a -> IO AppInfo)
  -> MVar AppModel
  -> [(ControllerSession a, MVar SessionRenderInfo)]
  -> PlayerState
  -> IO ()
onNewPlayerPos  getAppInfo appModelVar sessions newPlayerState = do
  curAppModel <- readMVar appModelVar
  let
    playPos = newPlayerState&getPlayerStep
    sharedModel = curAppModel&getCurSharedModel
  sessions&mapM_ (\(session, rendVar) -> do
      (_, _, contMod) <- readMVar $ session^.sUpdVar
      appInf <- getAppInfo session
      let
        curModel = (sharedModel, contMod)
        curMode = appMode $ (contMod^.aMode)
        funcs = session^.sConf.sFuncs
        outEv = (curMode^.renderPlayPosition) appInf (contMod^.conInp) curModel funcs
      tryPutMVar rendVar (Nothing, Just outEv)
    )


sessionRenderLoop ::
  (ControllerFunctions
  ,[MidiEvent] -> IO a
  ,[RenderEntry]
  , MVar (Maybe [RenderEntry]
  , Maybe [RenderEntry]))
  -> [RenderEntry]
  -> ([RenderEntry], [RenderEntry])
  -> IO b
sessionRenderLoop (funcs, renderFun, clearRenderFun, rendVar) lastRenderedEvents (keptEvents, keptOverlays) = do
  (newKeepEventsMaybe, overlayEventsMaybe) <- takeMVar rendVar
  let
    newKeepEvents = newKeepEventsMaybe&fromMaybe keptEvents
    newOverlayEvents = overlayEventsMaybe&fromMaybe keptOverlays
    outEvs = clearRenderFun&map (\(k,v) ->
        (lookup k newKeepEvents)&maybe
        (k,v)
        (\x -> (k,x))
      )

    outEvs2 = outEvs&map(\(k,v)->
        (lookup k newOverlayEvents)&maybe
        (k,v)
        (\x -> (k,x))
      )

    outSendEvs = outEvs2&mapMaybe(\(k,v) ->
        lookup k lastRenderedEvents&maybe
        (Just (k,v))
        (\x -> if x == v then Nothing else Just (k,v))
      )
    outMidiEvs =
      outSendEvs
      & map (\(cell, col) -> (funcs&setCellColor) col cell)
  _ <- renderFun (map snd outMidiEvs)
  threadDelay sessionRenderDelay
  sessionRenderLoop (funcs, renderFun, clearRenderFun, rendVar) outEvs2 (newKeepEvents, newOverlayEvents)


processSyncEvents ::
  PlayerState
  -> [MidiEvent]
  -> [MidiVoiceEvent]
  -> [TransportAction]
  -> (PlayerState, [MidiVoiceEvent], [TransportAction])
processSyncEvents playerState events accumOutEvents accumActs =
  case events of
    [] -> (playerState, accumOutEvents, accumActs)
    e:_ ->
      let
        (newPlayerState, outEvents, acts)= handleSyncEvent playerState (Just e)
      in
        processSyncEvents newPlayerState (drop 1 events) (outEvents ++ accumOutEvents) (acts++accumActs)


playLoop ::
  MVar PlayerState
    -> MVar PlayerRunnerState
    -> ([MidiVoiceEvent]
    -> IO a) -> TransportActionRunner
    -> MVar (Maybe PlayerEvents)
    -> (PlayerState -> IO b)
    -> IO Int
    -> Int
    -> IO ()
playLoop playerStateVar playRunnerVar playEvents transportRunner playerEventsVar onNewBeat bpmGetter tickCount = do
  threadDelay playLoopDelay
  curTime <- getTime Monotonic
  curBpm <- bpmGetter
  ticks <- modifyMVar playRunnerVar (pure . (stepPlayerRunner (stepsPerBeat*playerTicksPerStep) curTime curBpm))
  let tickSum = tickCount + ticks
  let sendTick = tickSum >= ticksPerSync
  let newTickCount = if sendTick then tickSum-ticksPerSync else tickSum
  when(sendTick) (
      transportRunner $ Tick 1
    )
  when (ticks>0) (do
    prevPlayerState <- readMVar playerStateVar
    newPlayerEventsMb <- swapMVar playerEventsVar Nothing
    (outEvents, transActions) <- modifyMVar playerStateVar (\playerState ->
        let
          (newPlayerState, outEvents, transActions) = tickPlayer ticks playerState
          updatedPlayerState = newPlayerEventsMb&maybe newPlayerState (\evs -> newPlayerState&playerSetEvents evs)
        in
          pure (updatedPlayerState, (concat outEvents, transActions))
      )

    newPlayerState <- readMVar playerStateVar
    _ <- when ((newPlayerState&getPlayerStep) /= (prevPlayerState&getPlayerStep))(
        onNewBeat newPlayerState >> return ()
      )

    unless(null outEvents) (
      playEvents outEvents >> return ()
      )

    transActions&mapM_ transportRunner
    return ()
    )
  playLoop playerStateVar playRunnerVar playEvents transportRunner playerEventsVar onNewBeat bpmGetter newTickCount


syncPlay ::
  TransportActionRunner
  -> IO [MidiEvent]
  -> ([MidiVoiceEvent] -> IO t)
  -> MVar PlayerState
  -> MVar (Maybe PlayerEvents)
  -> (PlayerState ->IO a)
  -> MVar (TimeSpec, TimeSpec)
  -> IO b
syncPlay transportRunner getSyncEvents playEvents playerStateVar playerEventsVar onNewPlayState tickDeltaVar = do
  newPlayerEventsMb <- swapMVar playerEventsVar Nothing
  playerState <- readMVar playerStateVar
  let updatedPlayerState = newPlayerEventsMb&maybe playerState (\evs -> playerState&playerSetEvents evs)
  syncEvents <- getSyncEvents
  let
    (newPlayerState, outEvents, transActions) = processSyncEvents updatedPlayerState syncEvents [] []

  -- TODO: enable again if we need play deltas
  -- curTime <- getTime Monotonic
  -- delta <- do
  --     (lastDelta, lastFullTick) <- readMVar tickDeltaVar
  --     if didClockTick then
  --       return $ diffTimeSpec curTime lastFullTick
  --     else
  --       return lastDelta
  -- _ <- swapMVar tickDeltaVar (delta, curTime)

  unless(null outEvents) (
      playEvents outEvents >> return ()
    )

  transActions&mapM_ transportRunner



  _ <- swapMVar playerStateVar newPlayerState
  _ <- when ((updatedPlayerState&getPlayerStep) /= (newPlayerState&getPlayerStep))(
      onNewPlayState newPlayerState >> return ()
    )
  threadDelay playLoopDelay
  syncPlay transportRunner getSyncEvents playEvents playerStateVar playerEventsVar onNewPlayState tickDeltaVar


