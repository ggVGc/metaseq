module Modes(
  appMode
  ,AppModeName(..)
  ,AppMode.update
  ,AppMode.render
  ,RenderEntry
  ,clearRender
) where

import AppMode
import AppModeName
import ArrangeMode
import SeqSelectMode
import SeqEditMode
import RootNoteMode
import LaneMode
import ControllerCommon
-- import DrumEditMode



appMode :: AppModeName -> AppMode
appMode name = case name of
  RootNoteMode -> (rootNoteMode StrongGreen)
  NoteMapSelectMode ->  noteMapSelectMode
  SeqSelectMode ->  (seqSelectMode StrongGreen)
  SeqEditMode -> seqEditMode
  ArrangeMode -> arrangeMode
  LaneMode -> laneMode
  -- DrumEditMode -> drumEditMode

