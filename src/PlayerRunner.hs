{-# LANGUAGE TemplateHaskell #-}

module PlayerRunner where

-- import Control.Monad
import Control.Lens
import System.Clock
import GHC.Int

maxSteps = 10

data PlayerRunnerState = PlayerRunnerState{
  _playing :: Bool
  ,_lastTickTime :: TimeSpec
  ,_restTime :: Integer
  ,_skipNextStep :: Bool
}
makeLenses ''PlayerRunnerState

defaultPlayerRunnerState = PlayerRunnerState {
  _playing = True
  ,_lastTickTime = TimeSpec 0 0
  ,_restTime = 0
  ,_skipNextStep = True
}


stepPlayerRunner :: Int -> TimeSpec -> Int -> PlayerRunnerState -> (PlayerRunnerState, Int)
stepPlayerRunner playerTicksPerBeat curTime bpm state =
  if (state^.skipNextStep) || (not $ state^.playing) then
    (state{_lastTickTime = curTime, _skipNextStep = False, _restTime = 0}, 0)
  else
    (newState, fromIntegral steps)
  where
    timePerBeat = toNanoSecs $ timeFromBPM bpm
    timePerStep = timePerBeat `div` (fromIntegral playerTicksPerBeat)
    addedTime = toNanoSecs $ diffTimeSpec (state^.lastTickTime) curTime
    totalTime =  addedTime + state^.restTime
    steps = min maxSteps (totalTime `div` timePerStep)
    newRest = totalTime `mod` timePerStep
    newState = state{_restTime = newRest, _lastTickTime = curTime}


timeFromBPM :: Int -> TimeSpec
timeFromBPM n =
  TimeSpec secs nanosecs
  where
    nanos = 1000*1000*1000 :: Int64
    x = (60*nanos) `div` (fromIntegral n)
    secs =  x `div` nanos
    nanosecs = (x - (secs*nanos))
