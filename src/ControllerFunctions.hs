{-# LANGUAGE LambdaCase #-}

module ControllerFunctions where
import Input
import ControllerCommon
import ZMidi.Core
import Misc


data ControllerFunctions = ControllerFunctions {
  viewWidth :: Int
  ,viewHeight :: Int
  ,rowPosTopCount :: Int
  ,rowPosSideCount :: Int
  ,cellFromKey :: MessageType -> Int -> Cell
  ,indexFromCell :: Cell -> Int
  ,gridFromIndex :: Int -> Cell
  ,setCellColor :: Color -> Cell -> (Cell, MidiEvent)
  ,controllerProcessEvent :: (MessageType -> Int -> Cell) -> MidiVoiceEvent -> InputEvent
}


runControllerProcessEvent funcs =
  (controllerProcessEvent funcs) (cellFromKey funcs)


transposeLaunchpad :: ControllerFunctions -> ControllerFunctions
transposeLaunchpad origFuncs = origFuncs{
  setCellColor = \col cell ->
    (setCellColor origFuncs) col (transposeCell cell)

  ,cellFromKey = \t m ->
    transposeInputCell $ (cellFromKey origFuncs) t m

  ,controllerProcessEvent = \getter e ->
    ((controllerProcessEvent origFuncs) getter  e)
    \> \case
        InDown cell -> InDown $ transposeCell cell
        InUp cell -> InUp $ transposeCell cell
        x -> x
  }
  where
    transposeCell =
      (invertCellY origFuncs)  . (invertCellX origFuncs)
      . \case
          GridXY x y -> invertCellX origFuncs $ GridXY y x
          RowPos(Side x) -> RowPos(Top x)
          RowPos(Top y) -> invertCellX origFuncs $ RowPos(Side y)
          x -> x

    transposeInputCell = invertCell origFuncs


invertCell funcs = (invertCellY funcs) . (invertCellX funcs)

invertCellY funcs = \case
  GridXY x y ->
    GridXY x (h-y-1)
  RowPos (Top y) ->
    RowPos $ Top (topCount - y -1)
  Shift -> Shift
  x@(RowPos(Side _)) -> x
  where
    topCount = rowPosTopCount funcs
    h = viewHeight funcs

invertCellX funcs = \case
  GridXY x y ->
    GridXY (w-x-1) y
  RowPos (Side x) ->
    RowPos $ Side (sideCount - x -1)
  Shift -> Shift
  x@(RowPos(Top _)) -> x
  where
    sideCount = rowPosSideCount funcs
    topCount = rowPosTopCount funcs
    w = viewWidth funcs


invertEvY funcs e =
  case e of
    InDown cell -> InDown $ invertCellY funcs cell
    InUp cell -> InUp $ invertCellY funcs cell
    _ -> e
