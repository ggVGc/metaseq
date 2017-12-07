module RootNoteMode where

import AppMode
import Control.Lens
import ControllerCommon
import Input
import Sequencer
-- import SeqConstants (sequenceDefaultLen)
import ControllerFunctions
import Data.Maybe(fromMaybe)
import Misc(traceThis, mapInd)
import qualified Data.IntMap.Strict as M
import Player
import SeqQueries(trackEntryIndexFromSongPos)


resolution :: Int
resolution = 8

rootNoteMode color = nopMode{
  _update = simpleUpdate $ rootNoteModeUpdate
  ,_render = rootNoteModeRender color
  ,_renderPlayPosition = \appInf input m funcs ->
    fromMaybe [] $ do
      let playPos = appInf^.appPlayerState&getPlayerStep
      (playingTe,ind) <- m^._1&trackEntryIndexFromSongPos (m^?curTrack) playPos
      if playingTe == (m^._2.selectedTrackPos) then
        Just [(GridXY (ind `div` resolution) 0, StrongAmber)]
      else Nothing

}



setRootNoteFromXY x y funcs model =
  model
  & curTrackEntry.rootNoteSeq %~ M.insert (x*resolution) v
  where
    v = (model^._2.rootCenterNote) - y+((funcs&viewHeight)`div`2)


rootNoteModeUpdate _ input oldModel funcs =
    case traceThis $ input^.ev of
      InDown (GridXY x y)-> Just $ oldModel&setRootNoteFromXY x y funcs
      InDown (RowPos ( Side 0))->
        Just $ oldModel&_2.rootCenterNote -~ 1
      InDown (RowPos ( Side 1))->
        Just $ oldModel&_2.rootCenterNote +~ 1
      InDown (RowPos (Top 6))->
        Just $ oldModel&_2.rootCenterNote %~ (\x -> x - (funcs&viewHeight)`div`2)
      InDown (RowPos ( Top 7))->
        Just $ oldModel&_2.rootCenterNote %~ (+ (funcs&viewHeight)`div`2)
      -- InDown(RowPos(Top x)) ->
      --   let maxPageIndex = sequenceDefaultLen`quot`(resolution*(funcs&viewWidth ))
      --   in
      --     Just $ oldModel&_2.rootSeqEditPage .~ (min x (maxPageIndex - 1))*resolution
      _ -> Nothing


rootNoteModeRender :: Color -> AppModeRender
rootNoteModeRender color _ input model funcs =
  concat [
    noteRenders
    -- ,[ ((RowPos $ Top $ model^._2.rootSeqEditPage `div` resolution), StrongRed)]
  ]
  where
    rootNote =
      model^._1
      & numToNote (model^._2.rootCenterNote)

    noteRenders = fromMaybe [] $ do
      te <- model^?curTrackEntry
      return $
        [0..(funcs&viewWidth)]
        & map(\x -> fromMaybe 0 (
            te^?rootNoteSeq.ix (x*resolution)
          ))
        & mapInd (\y x ->
          let
            outY =  y -1 +(funcs&viewHeight)`div`2 - (model^._2.rootCenterNote)
            maxY = (funcs&viewHeight) - 1
            outCol =
              if outY < 0 || outY > maxY then StrongAmber
              else color
            constrainedY = max 0 (min maxY outY)

          in
             (invertCellY funcs $ GridXY x constrainedY, outCol)
        )


