module Launchpad(
  launchpadFunctions
)
where

import ZMidi.Core
import Data.Bits
import ControllerCommon
import Misc
import Input
import ControllerFunctions(ControllerFunctions(ControllerFunctions))


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
    CC -> RowPos $ Top $ key-0x68
    Note -> case x of
      8 -> RowPos $ Side (7-y)
      _ -> GridXY x y
      where
        x = 0x0F .&. key
        y = shift (0xF0 .&. key) (-4)



makeKeyFromGridXY x y =
  (0x10 * y) + x


{-
   indexFromGrid g =
     case g of
       GridXY x y -> x+y*8
-}


indexFromCell :: Cell -> Int
indexFromCell g =
  case g of
    GridXY x y -> x+y*8
    RowPos (Top x) -> 64+x
    RowPos (Side x) -> 64+8+(7-x)
    Shift -> 64+16+1

gridFromIndex :: Int -> Cell
gridFromIndex ind = GridXY x y where
  x = ind `mod` 8
  y = floor $ fromIntegral ind / 8

makeKeyFromRowIndex i =
  case i of
    Side x -> checkIndex x ((0x10 * (7-x)) + 8)
    Top x -> checkIndex x (0x68 + x)
    where
      checkIndex _ {- ind -} v =
          -- assert (ind>=0 && ind<8 )
          v


makeColorVelocityData copyBit clearBitVal red green =
  -- assert (red>=0 && red<4)
  -- assert (green>=0 && green<4)
  (green`shift`4) .|. (clearBitVal`shift`3) .|. (copyBit`shift`2) .|. red




makeColorMessage :: Int -> Color -> MessageType -> MidiEvent
makeColorMessage key col messageType =
    case getRedGreenFromColor col of
      (r,g) -> makeChannelMessage key (makeColorVelocityData 1 1 r g) messageType



setCellColor :: Color -> Cell -> (Cell, MidiEvent)
setCellColor col cell =
    (cell, makeColorMessage key col t)
    where
      (t, key) =
        case cell of
          Shift -> (Note, makeKeyFromGridXY 0 7 )
          GridXY x y -> (Note, makeKeyFromGridXY x y)
          RowPos i -> case i of
            Side _ -> (Note, k)
            Top _ -> (CC, k)
            where
              k = makeKeyFromRowIndex i


processInp :: Int -> Cell -> InputEvent
processInp v cell =
  if v>10 then
    InDown c
  else
    InUp c
  where
    c = case cell of
      GridXY 0 7 -> Shift
      x -> x

controllerProcessEvent cellGetter  inEvent =
  case inEvent of
    Controller _ k v -> processInp (fromIntegral v) $ cellGetter ControllerCommon.CC (fromIntegral k)
    NoteOn _ k v -> processInp (fromIntegral v) $ cellGetter ControllerCommon.Note (fromIntegral k)
    NoteOff _ k _ -> processInp 0 $ cellGetter ControllerCommon.Note (fromIntegral k)
    unhandled -> NoInput `debug` ("Unhandled midi event(We shouldn't be using any of these): "++show unhandled)




launchpadFunctions = ControllerFunctions
  viewWidth
  viewHeight
  rowPosTopCount
  rowPosSideCount
  cellFromKey
  indexFromCell
  gridFromIndex
  setCellColor
  controllerProcessEvent
