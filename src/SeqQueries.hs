module SeqQueries where

import SeqTypes
-- import Misc((\>))
import Control.Lens
import SeqLenses
import Misc
import Data.Maybe
import Control.Arrow
import qualified Data.IntMap.Strict as M
import SeqConstants(sequenceDefaultLen)


-- laneIndexWithLockedNote :: NoteEntry -> S.Seq TrigSeqLane -> Maybe Int
-- laneIndexWithLockedNote note lanes =
--   lanes
--   \> S.findIndexL (\l -> l^.laneLockedNote == note)



getOccupiedLanes :: SeqModel -> [(TrigSeqLane, Int)]
getOccupiedLanes m =
  m^?curTriggerVariation
  \> maybe [] (
    (view trigEntryLanes)
    >>> M.assocs
    >>> mapMaybe(\(ind, lane) ->
        if(lane^.laneEntries == mempty) then
          Nothing
        else
          Just (lane, ind)
      )
    )


curTrackHasEntry :: Int -> SeqModel -> Bool
curTrackHasEntry ind m =
  m^?curTrack&maybe False (\track ->
    trackHasEntry ind track
  )


trackHasEntry :: Int -> ArrTrack -> Bool
trackHasEntry ind track =
  isJust $ (track^.trackEntries)^?ix ind



getTrackEntryLen :: Maybe TrackEntry -> SharedModel -> Int
getTrackEntryLen mbTe m = fromMaybe sequenceDefaultLen $ do
  te <- mbTe
  trigSeq <- m^?triggerSeqGroups.trigSeqVariation (te^.seqId)
  return $ (trigSeq&getSeqLength)



trackEntryIndexFromSongPos :: Maybe ArrTrack -> Int -> SharedModel -> Maybe (Int, Int)
trackEntryIndexFromSongPos mbTrack playPos m = do
  track <- mbTrack
  (i, _) <- track^.trackEntries&M.lookupLE playPos
  return (i, playPos-i)

songPosFromTrackEntry :: Int -> Int -> SeqModel -> Int
songPosFromTrackEntry trackInd trackEntryIndex m =
  [0..(trackEntryIndex-1)]
  & ffoldl 0 (\accum teInd ->
    let len =
          mbTrackEnts&maybe sequenceDefaultLen (\trackEnts ->
            getTrackEntryLen (trackEnts^?ix teInd) (m^._1)
          )
    in accum + len
  )
  where
    mbTrackEnts = m^?curArrangement.ix trackInd.trackEntries


curPlayingTriggerIndex :: Int -> SeqModel -> Maybe Int
curPlayingTriggerIndex playPos m = do
  (playingTrackEntIndex, trackTriggerInd) <- trackEntryIndexFromSongPos (m^?curTrack) playPos (m^._1)
  curId <- m^?curSeqId
  playingId <- m^?curTrack.trackEntries.ix playingTrackEntIndex.seqId
  sq <- m^?_1.triggerSeqGroups.trigSeqVariation curId
  if curId == playingId then
    Just $ trackTriggerInd `rem` (sq&getSeqLength)
  else
    Nothing




