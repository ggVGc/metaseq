
module SeqConstants where

velPerStep = 17 :: Int

sequenceDefaultLen = 64 :: Int
trackCount = 70 :: Int -- Arbitrary number, and should be a Map instead
trackPageSize = 64 -- How many TrackEntries on one page of a track, should also be a Map..
trackPageCount = 1 :: Int
trackLength = trackPageCount * trackPageSize -- How many sequences per track
trackNoteCount = trackLength * sequenceDefaultLen -- How many indicidual notes in one track
defaultSeqCount = 32 :: Int
defaultNoteEntryCount = 24 :: Int
trigLaneCount = 127 :: Int -- Max note value in midi
defaultLoopLen = 64 :: Int
baseDrumNote = 60 :: Int -- double entendre..
playerTicksPerStep = 60 :: Int
stepsPerBeat = 4 :: Int
ticksPerSync = 10 :: Int

defaultVelocity :: Int
defaultVelocity = 100

minVelocity = 50 :: Int
maxVelocity = 127 :: Int
maxVelStep = 7 :: Int
velStepSize = (maxVelocity-minVelocity) `div` maxVelStep :: Int
velForStep i =
  if i == maxVelStep then maxVelocity
  else
    minVelocity + velStepSize*i


defaultNoteMap :: [[Int]]
defaultNoteMap = [
    -- [ 0,2,4,5,7,9,11 ] C major
    -- [2, 4, 5, 7, 9, 10, 12] -- D minor
    -- [3, 5, 6, 8, 10, 11, 12] -- D# minor
    [0,1,2,3,4,5,6,7,8,9,10,11]
  ]

identityNoteMap :: [Int]
identityNoteMap = [0,1,2,3,4,5,6,7,8,9,10,11]
