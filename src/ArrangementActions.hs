module ArrangementActions where

import Control.Lens((&), (.~), (^.), (^?), _1, _2, (%~), ix)
import SeqLenses
import SeqTypes
import Sequencer
import AppMode
import ControllerFunctions
import Misc((\>), boundedAdd, toggleMapIndex)
import Data.Foldable(toList)
import Data.Maybe(fromMaybe)
import SeqQueries
import qualified Data.IntMap.Strict as M
import Player

focusPlayPosCurrentTrack :: ControllerFunctions -> AppInfo -> SeqModel -> SeqModel
focusPlayPosCurrentTrack funcs appInf m = fromMaybe m $ do
  track <- m^?curTrack
  curEntryInd <- (m^._1&trackEntryIndexFromSongPos (Just track) (appInf^.appPlayerState&getPlayerStep))
  let center = (funcs&viewWidth)`div`2 -1
  return $ m&_2.arrViewOffset._1 .~ max (curEntryInd^._1 - center ) 0

stepViewOffset :: Int -> SeqModel -> SeqModel
stepViewOffset step m = fromMaybe m $ do
  tracks <- m^?curArrangement
  (longestLen, _) <- (tracks \> toList \> longestTrack)
  return $
    m&_2.arrViewOffset._1 %~ boundedAdd 0 longestLen step


toggleTrackEntry :: Int -> Int -> SeqModel -> SeqModel
toggleTrackEntry trackInd ind m =
  m&curArrangement.ix trackInd.trackEntries %~ toggleMapIndex ind entry
  where
    entry =
      (m^._1)
      & firstEmptySeq trackInd
      & fromMaybe defaultTrackEntry


copyCurTrackEntryTo :: Int -> Int -> SeqModel -> SeqModel
copyCurTrackEntryTo ind trackInd m = fromMaybe m $ do
  curEntry <- m^?curTrackEntry
  return $ m&curArrangement.ix trackInd.trackEntries %~ M.insert ind curEntry



selectIfValid :: Int -> Int -> SeqModel -> SeqModel
selectIfValid trackInd ind m = fromMaybe m $ do
  track <- m^?curArrangement.ix trackInd
  if track&trackHasEntry ind then
    m
    & selectTrackEntry ind
    & _2.curTrackIndex .~ trackInd
    & Just
  else
    Nothing


selectTrackEntry :: Int -> SeqModel -> SeqModel
selectTrackEntry ind =
  (_2 %~ (
      selectedTrackPos .~ ind)
      . (rootSeqEditPage.~0)
    )
