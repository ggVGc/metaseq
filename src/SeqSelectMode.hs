module SeqSelectMode where


import Control.Lens
import Sequencer
import GenericModes(gridSelectMode)


seqSelectMode = gridSelectMode (curTrackEntry.seqId._1)

seqVariationSelector = gridSelectMode (curTrackEntry.seqId._2)
