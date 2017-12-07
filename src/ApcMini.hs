
module ApcMini(
  apcMiniFunctions
)where

import ZMidi.Core
-- import Data.Word
import ControllerCommon
import Input
import ControllerFunctions(ControllerFunctions(ControllerFunctions))
import Misc


{-
   resetByte :: Word8
   resetByte = 0xB0
-}

viewWidth :: Int
viewWidth = 8

viewHeight :: Int
viewHeight= 8

rowPosTopCount :: Int
rowPosTopCount = 8

rowPosSideCount :: Int
rowPosSideCount = 8

cellFromKey :: MessageType -> Int -> Cell
cellFromKey messageType key =
  case messageType of
    CC ->
      if key < 64 then
        GridXY x y
      else
        onCC key
    Note ->
      if key < 64 then case x of
        8 -> RowPos $ Side $ rowPosSideCount - 1 - y
        _ -> GridXY x y
      else
        onCC key
    where
      x = key `mod` 8
      y = 7-(key `quot` 8)
      onCC srcKey =
        let k = srcKey -64
        in if k<8 then
          RowPos $ Top k
        else
          RowPos $ Side (rowPosSideCount - 1 - (k-18))


makeKeyFromGridXY :: Int -> Int -> Int
makeKeyFromGridXY x y =
  8*(7-y) + x


indexFromCell :: Cell -> Int
indexFromCell g =
  case g of
    GridXY x y -> x+y*8
    RowPos (Top x) -> 64+x
    RowPos (Side x) -> 64+8+x
    Shift -> 64+16+1


gridFromIndex :: Int -> Cell
gridFromIndex ind = GridXY x y where
  x = ind `mod` 8
  y = ind `div` 8


makeColorVelocityData :: Color -> Int
makeColorVelocityData col =
  case getRedGreenFromColor col of
    (0, 0) -> 0
    (0, 3) -> 1
    (0, _) -> 2
    (3, 0) -> 3
    (_, 0) -> 4
    (3,3) -> 5
    _ -> 6


makeColorMessage :: Int -> Color -> MessageType -> MidiEvent
makeColorMessage key col =
  makeChannelMessage key (makeColorVelocityData col)


setCellColor :: Color -> Cell -> (Cell, MidiEvent)
setCellColor  col cell =
    (cell, makeColorMessage key col t)
    where
      (t, key) =
        case cell of
          Shift -> (Note, 98)
          -- Shift2 -> (Note, 71)
          GridXY x y -> (Note, makeKeyFromGridXY x y)
          RowPos i -> case i of
            Side x -> (Note, rowPosSideCount - 1 - x+82)
            Top x -> (Note, x+64)


controllerProcessEvent cellGetter inEvent =
  case inEvent of
    Controller _  98 _ -> inp 0 Shift -- alsa gives me controller..
    NoteOff _  98 _ -> inp 0 Shift -- Portmidi gives me a noteoff..
    -- Controller _  71 _ -> inp 0 Shift2
    Controller _  k v ->
      let cell = cellGetter ControllerCommon.CC (fromIntegral k)
      in if k >= 64 then
        inp 0 cell
      else case cell of
        GridXY _ _ -> inp 0 cell
        _ -> inp v cell

    NoteOn _ 98 v -> inp v Shift
    NoteOn _ k v -> inp v $ cellGetter ControllerCommon.Note (fromIntegral k)
    NoteOff _ k _ -> inp 0 $ cellGetter ControllerCommon.Note (fromIntegral k)
    unhandled -> NoInput `debug` ("Unhandled midi event(We shouldn't be using any of these): "++show unhandled)
  where
    inp v cell =
      if v>10 then case cell of
        Shift -> InDown cell
        -- Shift2 -> InDown cell
        GridXY _ _ -> InDown cell
        RowPos(Side _) -> InDown cell
        RowPos(Top x) ->
          if x < 8 then InDown cell
          else InUp cell
      else InUp cell


apcMiniFunctions = ControllerFunctions
  viewWidth
  viewHeight
  rowPosTopCount
  rowPosSideCount
  cellFromKey
  indexFromCell
  gridFromIndex
  setCellColor
  controllerProcessEvent


