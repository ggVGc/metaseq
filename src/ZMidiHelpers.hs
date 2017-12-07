module ZMidiHelpers where

import ZMidi.Core
import Control.Arrow
import Control.Lens((&))
import Data.Bits
import Sequencer(defaultNoteEntry, numToNote')
import SeqTypes(NoteEntry(..))
import Misc
import GHC.Word



setChannelForEvents :: [[MidiVoiceEvent]] -> [MidiVoiceEvent]
setChannelForEvents  =
  mapInd (\evs chan ->
    evs&map (\e -> case e of
      NoteOn _ n v -> NoteOn chan n v
      NoteOff _ n v -> NoteOff chan n v
      NoteAftertouch _ n v -> NoteAftertouch chan n v
      Controller _ n v -> Controller chan n v
      ProgramChange _ v -> ProgramChange chan v
      ChanAftertouch _ v -> ChanAftertouch chan v
      PitchBend _ v -> PitchBend chan v
    )
  )
  >>> concat



toSongPos :: Int -> Int -> Int
toSongPos highBits lowBits =
  ((traceThis lowBits)`shiftL` 7) + (traceThis highBits)

fromSongPos :: Int -> (Word8, Word8)
fromSongPos pos =
  let
    high = pos `shiftR` 7
    low = pos - (high`shiftL`7)
  in
    (fromIntegral high, fromIntegral low)



eventToNote :: MidiVoiceEvent -> NoteEntry
eventToNote ev =
  case ev of
    NoteOn _ k _ -> numToNote' 12 (fromIntegral k)
    NoteOff _ k _ -> numToNote' 12 (fromIntegral k)
    _ -> defaultNoteEntry `debug` ("eventToNote: Unhandled event, used default:"++show ev)

