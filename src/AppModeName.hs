
module AppModeName where


data AppModeName =
  SeqEditMode
  |RootNoteMode
  |NoteMapSelectMode
  |SeqSelectMode
  |ArrangeMode
  |LaneMode
  -- |DrumEditMode
  deriving (Show, Read, Enum, Ord, Eq)

