{-# LANGUAGE LambdaCase #-}

module Transport where

import ZMidi.Core
import ZMidiHelpers


data TransportAction =
  SetSongPos Int
  | SetLoopBegin Int
  | SetLoopEnd Int
  | StartLoop
  | StopLoop
  | Tick Int
  deriving (Show, Read, Eq)



runTransportAction :: ([MidiEvent] -> IO a) -> TransportAction -> IO ()
runTransportAction writer = \case
  SetSongPos pos ->
    let (high, low) = fromSongPos pos
    in
      writer [
            SysCommonEvent $ SongPosPointer low high
          ]
      >> return ()
  Tick ticks ->
    writer [
        SysRealTimeEvent TimingClock
      ]
    >> return ()
  _ -> return ()


