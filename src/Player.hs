{-# LANGUAGE LambdaCase #-}
module Player(
  PlayerState
  ,defaultPlayerState
  ,playerSetEvents
  ,playerResetPos
  ,tickPlayer
  ,playerSetPos
  ,getPlayerStep
  ,looping
  ,handleSyncEvent
  ,playerTransformEvents
  ,setLoopEnd
  ,setLoopStart
  ,playerGetLoopEnd
  ,playerGetLoopStart
  ,playerReturnToLoopStart
  ,stopAllChannelNotes
  ,stopAllNotes
  ,_playerPos
  ,PlayerEvents
) where
import Data.Maybe
import Control.Arrow
import Control.Lens((&), ix, firstOf)
import SequenceRender
import ZMidi.Core
import Misc
import ZMidiHelpers
import Data.Vector (Vector, empty, fromList)
import qualified Data.IntMap.Strict as M
import Control.Monad.State
import Data.Foldable(toList)
import SeqConstants
import Transport
import GHC.Word


type PlayerEvents = Vector(M.IntMap [MidiVoiceEvent])

data PlayerState = PlayerState {
  _playerPos :: Int
  ,events :: PlayerEvents
  ,loopStart :: Int
  ,loopEnd :: Int
  ,looping :: Bool
  ,didLoop :: Bool
} deriving (Show)


getPlayerStep st =
  (st&_playerPos) `div` playerTicksPerStep

playerGetLoopEnd st = (st&loopEnd) `div` playerTicksPerStep
playerGetLoopStart st = (st&loopStart) `div` playerTicksPerStep


defaultPlayerState :: PlayerState
defaultPlayerState = PlayerState 0 empty 0 (sequenceDefaultLen*playerTicksPerStep) True False


playerTransformEvents :: SongEvents -> Vector (M.IntMap [MidiVoiceEvent])
playerTransformEvents evs =
  evs
  \>map (\(chan, trackEvs) ->
    trackEvs
    \> flip M.foldlWithKey' M.empty (\accum ind posEvents ->
        flip evalState accum (do
          posEvents \> mapM_(\e -> do
                let (ev, offset) = unpackNoteOut (fromIntegral chan) e
                m <- get
                put $ adjustMapWithDefault [] (ind*playerTicksPerStep + offset) ((:) ev) m
            )
          get
        )
      )
    )
  \> fromList
  where
    unpackNoteOut chan = \case
      NoteBegin d -> makeNoteEv NoteOn chan d
      NoteEnd d -> makeNoteEv NoteOff chan d

    makeNoteEv constructor chan (k,v,offset) =
      (constructor chan (fromIntegral k) (fromIntegral v), offset)

playerSetEvents newEvents st =
  st{events=newEvents}


playerResetPos :: PlayerState -> PlayerState
playerResetPos = playerSetPos 0


playerSetPos:: Int -> PlayerState -> PlayerState
playerSetPos newPos st =
  st{_playerPos=newPos*playerTicksPerStep}

playerReturnToLoopStart :: PlayerState -> PlayerState
playerReturnToLoopStart st =
  st{_playerPos=st&loopStart}


_tickPlayer :: Int -> (PlayerState, [[MidiVoiceEvent]], [TransportAction]) -> (PlayerState, [[MidiVoiceEvent]], [TransportAction])
_tickPlayer tickCount (st, evs, actions) =
  if tickCount == 0 then
    (st, evs, actions)
  else
    let
      (newSt, newEvs, newActs) = tickPlayerOnce st
    in
      _tickPlayer (tickCount-1) (newSt, newEvs++evs, newActs++actions)


tickPlayer :: Int -> PlayerState -> (PlayerState, [[MidiVoiceEvent]], [TransportAction])
tickPlayer tickCount st =
  _tickPlayer tickCount (st, [], [])


tickPlayerOnce :: PlayerState -> (PlayerState, [[MidiVoiceEvent]], [TransportAction])
tickPlayerOnce st =
  (newState{didLoop = shouldLoop},  newEvs, act)
  where
    shouldLoop =
      (st&looping) && (st&_playerPos) + 1 == (st&loopEnd)

    -- stopEvs =
    --   if shouldLoop then stopAllNotes
    --   else []
    --
    -- outEvs = stopEvs : newEvs

    newPos =
      if (st&didLoop) then
        st&loopStart
      else
        (st&_playerPos)+1
    newState = st{_playerPos = newPos}
    newEvs =
      st\>events\>toList
      \> map(
          (firstOf $ ix (_playerPos st))
          >>> fromMaybe []
        )
    act =
      if shouldLoop then
        [SetSongPos $ (st&loopStart)`div`playerTicksPerStep]
      else []


handleSyncEvent :: PlayerState -> Maybe MidiEvent -> (PlayerState,  [MidiVoiceEvent], [TransportAction])
handleSyncEvent playerState syncEvent =
  let
    unhandledEvent = (playerState, [], []) -- `debug` ("Unhandled event: "++show x)
  in case syncEvent of
    Just (SysRealTimeEvent evt) ->
      case evt of
        TimingClock ->
            let
              (newPlayerState, outEvents, acts) =  tickPlayerOnce playerState
            in (newPlayerState, concat outEvents, acts)
        StartSequence ->
          let (newPlayerState, outEvents, acts) =  (tickPlayerOnce . playerResetPos) playerState
          in (newPlayerState, concat outEvents, acts) `debug` "Start"
        StopSequence ->
          (playerState, stopAllNotes, []) `debug` "stop"
        ContinueSequence ->
          (playerState, [], []) `debug` "continue"
        SystemReset ->
          (playerResetPos playerState, stopAllNotes, []) `debug` "reset"
        _ -> unhandledEvent
    Just (SysCommonEvent evt) ->
      case evt of
        SongPosPointer low high ->
          let
            newPos = (toSongPos (fromIntegral low) (fromIntegral high))
          in
            ((playerSetPos $ traceThisM "songpos: " (fromIntegral newPos)) playerState, stopAllNotes, [])
        _ -> unhandledEvent
    Just _ -> unhandledEvent
    Nothing ->
        (playerState, [], [])


stopAllNotes :: [MidiVoiceEvent]
stopAllNotes =
  [0..15]
  \> map stopAllChannelNotes
  \> concat


stopAllChannelNotes :: Word8 -> [MidiVoiceEvent]
stopAllChannelNotes chan =
  [0..127]
  \> map (\x -> NoteOff chan x 64)


setLoopStart :: Int -> PlayerState -> PlayerState
setLoopStart pos st =
  st{loopStart = pos*playerTicksPerStep}


setLoopEnd :: Int -> PlayerState -> PlayerState
setLoopEnd pos st =
  st{loopEnd = pos*playerTicksPerStep}


