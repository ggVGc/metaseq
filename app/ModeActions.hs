module ModeActions where

import Control.Concurrent
import Control.Lens
import Recorder
import Application
import SeqTypes
import ZMidi.Core
import GHC.Word
import SeqConstants(defaultVelocity)
import Player

play ::  (Word8 -> Word8 -> Word8 -> MidiVoiceEvent) -> Word8 -> Word8 -> ([MidiVoiceEvent] -> IO a) -> SeqModel -> IO()
play con n v playNotes m =
  (playNotes $ [con (fromIntegral $ m^._2.curTrackIndex) n v]) >> return ()
-- (clockTickDelta, lastClockTime) <- readMVar tickDeltaVar


playNote =
    -- last <- swapMVar lastPlayedNoteVar (Just n)
    -- last\>mapM_ (play NoteOff)
    play NoteOn


stopPlayNote n =
    -- _ <- swapMVar lastPlayedNoteVar Nothing
    play NoteOff n (fromIntegral defaultVelocity)


beginRecNote n v = beginRecNotes [(n,v)]

beginRecNotes noteVals recorderVar playerStateVar curModel = do
    -- print ("REC "++show n++", "++show v)
    playerState <- readMVar playerStateVar
    modifyMVar_ recorderVar (
        pure . (startRecordNotes curModel playerState noteVals)
      )
    return ()


stopRecNote n = stopRecNotes [n]

doRecorderUpdate :: ((SharedModel, t) -> a1 -> a -> (a, SharedModel)) -> MVar a1 -> MVar AppModel -> MVar a -> t -> IO ()
doRecorderUpdate f playerStateVar appModelVar recorderVar contMod = do
    curAppModel <- readMVar appModelVar
    newSharedModel <- modifyMVar recorderVar (\st -> do
        playerState <- readMVar playerStateVar
        let (newRecState, newModel) = f (curAppModel&getCurSharedModel, contMod) playerState st
        return $ (newRecState, newModel)
      )
    _ <- swapMVar appModelVar (curAppModel&curSharedModel .~ newSharedModel)
    -- _ <- songRenderer
    return ()

stopRecNotes noteVals =
  doRecorderUpdate (stopRecordNotes noteVals)

stopAllRec :: MVar PlayerState -> MVar AppModel -> MVar RecorderState -> ControllerState -> IO ()
stopAllRec =
  doRecorderUpdate stopRecording
