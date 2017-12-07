{-# LANGUAGE TemplateHaskell #-}

module Recorder where
import Control.Lens
import SeqTypes
import SeqConstants
import Misc
import Sequencer
import qualified Data.IntMap.Strict as M
import Data.IntMap (lookupLE)
import Data.List
import SeqEditActions (deleteContinuationsFrom)
import LaneActions
import Player
import Data.Maybe
import Control.Arrow
import Data.Monoid((<>))

type StartInfo = (Int, NoteEntry,Int) -- delay, note, vel

data RecorderState = RecorderState {
  _activeRecs :: M.IntMap [StartInfo] -- startTrigIndex -> [info]
  ,_startedNotes :: [NoteEntry]
  ,_firstRecPlayPos :: Maybe Int
}

defaultRecorderState = RecorderState M.empty [] Nothing

recLoopQuantizeSize = 8

makeLenses ''RecorderState

startRecordNotes :: SeqModel -> PlayerState -> [(Int, Int)] -> RecorderState -> RecorderState
startRecordNotes model playerState noteVals st =
  -- (makeLanes, st&activeRecs %~ (\recs -> newRecs&foldl' folder recs))
  st&(
      (activeRecs %~ (\recs -> newRecs&foldl' folder recs))
      . (startedNotes %~ (<> (map fst notes)))
      . (firstRecPlayPos %~ maybe (Just $ playerState&getPlayerStep) (Just . id))
    )

  where
    notes = noteVals&map(\(n,v)->(numToNote' 12 n, v))
    -- makeLanes =
    --   notes&ffoldl model (\m (note, _) ->
    --     m&editLane (getLaneIndex note m) id -- Make sure lanes for each note exist
    --   )
    folder recs (i, v) =
      adjustMapWithDefault [] i ((:)v) recs

    newRecs =
      notes&map(\(n,v)->
          let (trigInd, noteDelay) = trigIndFromPlayPos playerState n model
          in (trigInd,(noteDelay, n, v))
      )


stopRecording :: SeqModel -> PlayerState -> RecorderState -> (RecorderState, SharedModel)
stopRecording oldModel playerState oldRecState =
  (stopRecordNotes [0..127] oldModel playerState oldRecState)
  \> updateLoopPoints
  \> (\(_,m) -> (defaultRecorderState, m))
  where
    contMod = oldModel^._2

    updateLoopPoints :: (RecorderState, SharedModel) -> (RecorderState, SharedModel)
    updateLoopPoints (recState, model) =
      (recState, newMod^._1)
      where
        seqModel = (model, contMod)
        newMod =
          (recState^.startedNotes)
          \> foldl' (\m n ->
              m
              \> editLane (getLaneIndex n m) (
                    laneLoopLen .~ getNewLoopLen playerState recState
                  )
            ) seqModel


stopRecordNotes ::
  [Int] ->
  SeqModel ->
  PlayerState ->
  RecorderState ->
  (RecorderState, SharedModel)
stopRecordNotes noteVals model playerState st =
  noteVals
  \> map (numToNote' 12)
  \> foldl' (\(s, m) n ->
      stopRecordNote m playerState n s
    ) (st, model)
  \> (\(s, m) -> (s, m^._1))


stopRecordNote:: SeqModel -> PlayerState -> NoteEntry -> RecorderState -> (RecorderState, SeqModel)
stopRecordNote model playerState note st =
  if null startInfos then (st, model)
  else
    model
    \> updateLane
    \> (\m -> (newState, m))
  where
    (endTrigInd, _) = trigIndFromPlayPos playerState note model
    startInfos = getStartInfos st note

    newState :: RecorderState
    newState = fromMaybe st $ do
      -- TODO: use startDelay
      (startInd, (startDelay, n, _)) <- find(snd >>> startInfoHasNote note) startInfos

      return $
        st&activeRecs.ix startInd %~(
            filter (not . (startInfoHasNote note))
          )

    updateLane :: SeqModel -> SeqModel
    updateLane = editLane (getLaneIndex note model) (\lane ->lane &
        (laneLoopLen .~ getNewLoopLen playerState newState)
        .(laneEntries %~ updateEntries (lane^.laneLoopLen))
      )

    updateEntries :: Int -> LaneEntries -> LaneEntries
    updateEntries loopLen oldEnts =
      startInfos
      \> foldl' (foldTriggerInfos endTrigInd) oldEnts
      \> deleteContinuationsFrom endTrigInd loopLen

getNewLoopLen :: PlayerState -> RecorderState -> Int
getNewLoopLen playerState  recState =
  traceThisM "new loop len: " $
    if (playerState&getPlayerStep) < firstRec then
      playerState&playerGetLoopEnd
    else
      (1 + (step `quot` recLoopQuantizeSize)) * recLoopQuantizeSize
    where
      firstRec = fromMaybe 0 (recState^.firstRecPlayPos)
      step = playerState&getPlayerStep


foldTriggerInfos :: Int -> LaneEntries-> (Int, StartInfo) -> LaneEntries
foldTriggerInfos endTrigInd ents (startTrigInd, (startNoteDelay, _, velocity)) =
  let
    newTrigEntry = TriggerEntry $
      defaultTriggerData
      \> vel .~ velocity
      \> delay .~ startNoteDelay
    realEndInd = if endTrigInd < startTrigInd then sequenceDefaultLen else endTrigInd
  in
    ents
    \> M.insert startTrigInd newTrigEntry
    \> (if realEndInd-startTrigInd <= 1 then id else modMapRange (startTrigInd+1) realEndInd (const $ Just ContinuationEntry))




getStartInfos :: RecorderState -> NoteEntry -> [(Int, StartInfo)]
getStartInfos st note =
  st^.activeRecs
  \> M.toList
  \> map(\(startTrigInd, noteInfos) ->
    noteInfos
    \> filter(startInfoHasNote note)
    \> map(\x -> (startTrigInd, x))
    )
  \> concat

startInfoHasNote :: Eq a => a -> (t, a, t1) -> Bool
startInfoHasNote note (_, n, _) = n == note

trigIndFromPlayPos :: PlayerState -> NoteEntry -> SeqModel -> (Int, Int)
trigIndFromPlayPos playerState note m = fromMaybe (0,0) $ do
  let
    playerStep = playerState&getPlayerStep
    trigInd = playerStep
    noteDelay = (playerState&_playerPos) - playerStep*playerTicksPerStep
  entries <- m^?curTrack.trackEntries
  (trigEntryInd, _) <- entries&lookupLE playerStep
  return (max 0 (trigInd-trigEntryInd), noteDelay)


getLaneIndex :: NoteEntry -> SeqModel -> Int
getLaneIndex note m =
  m&trackNoteToNum note
