{-
   module NoteMode where
   
   -- This module is no longer used. Replaced by LaneMode,
   -- since lanes can no longer hold different note values
   
   
   import AppMode
   import Data.Maybe
   import Sequencer
   import Control.Lens
   import Misc
   import Input
   import ControllerCommon
   import Control.Arrow
   import SeqEditMode(render, update', viewNote, editNoteNum, editNoteOctave, renderPlayPos)
   import Data.List as L
   import ControllerFunctions
   
   modKeySeqEdit = RowPos $ Top $ 7
   
   
   noteMode = AppMode NoteMode.renderPlayPos  NoteMode.metaUpdate NoteMode.metaRender
   
   
   renderPlayPos :: PlayPosRender
   renderPlayPos frameInfo inp m funcs=
     if inp&keyDown (RowPos(Top 7)) then
       SeqEditMode.renderPlayPos frameInfo inp m funcs
     else
       m&curPlayingTriggerIndex (frameInfo^.playerPos)&maybe [] (\playIndex ->
           let
             len notes = length notes - 1 - (m&editNoteOffset)
             processNotes =
               reverse
               >>> listToMaybe
               >>> fmap (view _1)
               >>> fromMaybe defaultTriggerData
   
             (trig,displayInd) =
               m&getCurActiveLaneNotes
               & fromMaybe []
               & takeWhile (\(_,_,x) -> x <= playIndex)
               & (\notes ->(processNotes notes, len notes))
   
             offset = (m&trackNoteToNum (m^._2.centerEditNote)) + 1
           in
             if (displayInd>=0 && displayInd < (funcs&viewWidth)) then
               [(funcs&setCellColor) StrongGreen (GridXY displayInd (noteRealY funcs m (offset - (m&trackNoteToNum (trig^.offsetNote)))))]
             else
               []
         )
   
   
   metaRender =
     metaRender' NoteMode.render
   
   
   metaRender':: AppModeRender -> AppModeRender
   metaRender' mainRenderFun appInf input m =
     if input&keyDown modKeySeqEdit then
       SeqEditMode.render appInf ( input&filterDowns [modKeySeqEdit]) m
     else
       mainRenderFun appInf input m
   
   
   selNoteRenderSingle funcs m =
     (m&getCurActiveLaneNotes)
     & maybe [] (
         L.drop (m&editNoteOffset)
         >>> L.take (funcs&viewWidth)
         >>> mapInd (\(trig, _, _) renderInd ->
           (uncurry (funcs&setCellColor)) (noteRender funcs m trig renderInd)
       ))
   
   
   noteRender :: ControllerFunctions -> SeqModel -> TriggerData -> Int -> (Color, Cell)
   noteRender funcs m t =
     noteSelectCommonRender funcs StrongRed (noteNum+1) m (t^.offsetNote)
     where
       noteNum = m&trackNoteToNum (m^._2.centerEditNote)
   
   
   render _ input m funcs =
     let
       sideRender =
         if input&keyDown Shift then []
         else
           let
             n = (m&viewNote funcs (m^._2.centerEditNote))^.octave
           in
             [(funcs&setCellColor) StrongRed (RowPos(Side $ n))]
       topRender =
         if input&keyDown Shift then
           [(funcs&setCellColor) StrongRed (RowPos $ Top $ (m^._2.centerEditNote.nNum))]
         else []
   
     in concat [
       selNoteRenderSingle funcs m
       , sideRender
       , topRender
     ]
   
   
   metaUpdate =
     metaUpdate' NoteMode.update
   
   metaUpdate' :: AppModeUpdate -> AppModeUpdate
   metaUpdate' mainUpdateFun appInf input m funcs =
     if input&keyDown modKeySeqEdit then
       let
         newInp = input&filterDowns [modKeySeqEdit]
         upd = SeqEditMode.update' False
       in
         (runOtherModeUpdate funcs upd appInf newInp m)
         & fmap(\(newMod, acts) -> (UpdateSingle, newMod, acts))
     else
       mainUpdateFun appInf input m funcs
   
   
   
   topUpdate funcs shiftHeld topInd m =
     if shiftHeld then
       m&_2.centerEditNote %~ editNoteNum funcs m topInd
     else case topInd of
       0 ->
         m&_2.selectedTriggerEntry %~ (\x -> m&prevSelectedNote&fromMaybe x)
       1 ->
         m&_2.selectedTriggerEntry %~ (\x -> m&nextSelectedNote&fromMaybe x)
       2 ->
         m&_2.centerEditNote %~ \n -> m&trackNumToNote((m&trackNoteToNum n)+1)
       3 ->
         m&_2.centerEditNote %~ \n -> m&trackNumToNote(max 0 (m&trackNoteToNum n)-1)
       _ -> m
   
   
   
   update :: AppModeUpdate
   update = simpleUpdate $ \_ input m funcs ->
         case input^.ev of
           InDown(RowPos(Top x)) ->
             Just $ m&topUpdate funcs (input&keyDown Shift) x
           InDown (RowPos(Side y)) ->
             Just $ m&_2.centerEditNote %~ editNoteOctave funcs m y
           InDown (GridXY x y) ->
               let
                 maybeNotes = m&getCurActiveLaneNotes
                 maybeInd = maybeNotes >>= (\activeNotes ->activeNotes&(
                     drop (m&editNoteOffset)
                     >>>drop x
                     >>> map third
                     >>> listToMaybe
                   ))
               in
                 maybeInd&fmap (\i -> updateTriggerEntry y i m)
           _ -> Nothing
   
   
   updateTriggerEntry :: Int -> Int -> SeqModel -> SeqModel
   updateTriggerEntry y ind m =
     m&laneEntry ind . _TriggerEntry %~ (\trig ->
         let
           noteNum = (m&trackNoteToNum (m^._2.centerEditNote)) - y + 1
         in
           trig&offsetNote .~ (m&trackNumToNote noteNum)
       )
-}
