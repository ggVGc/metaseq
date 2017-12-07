module NetworkPlay where
import Network.Simple.TCP
import ZMidi.Core
-- import ZMidiHelpers
import Control.Lens ((&))
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
-- import Player
-- import SequenceRender
-- import Misc
import Data.Monoid
import Control.Monad

packMsg :: Int -> MidiVoiceEvent -> BL.ByteString
packMsg ind msg =
  case msg of
    NoteOn c k v ->
      (int8 (fromIntegral $ c)
      <> int8 (fromIntegral k)
      <> int8 (fromIntegral v)
      <> int16BE (fromIntegral ind)
      <> int16BE (fromIntegral (ind+1)))
      & toLazyByteString

    _ ->
      BL.empty

sendReset :: Socket -> IO()
sendReset sock =
  let
    bs =
      (int8 (fromIntegral $ 255)
      <> int8 0
      <> int8 0
      <> int16BE 0
      <> int16BE 0)
      & toLazyByteString
  in
    sendLazy sock bs

sendIfNotEmpty :: Socket -> BL.ByteString -> IO ()
sendIfNotEmpty sock bytes =
  unless (bytes == BL.empty)(
      sendLazy sock bytes
    )

-- sendTrackEvents :: Socket -> [[NoteOut]] -> IO ()
-- sendTrackEvents sock evs =
--   evs&renderNoteOuts&mapInd(,)&mapM_(\(frameEv, ind) ->
--       packMsg ind frameEv
--       & sendIfNotEmpty sock
--     )
--


