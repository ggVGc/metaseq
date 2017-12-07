{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Input where
-- import Misc

import ControllerCommon
import Data.Maybe
import Control.Lens
import Data.List
import qualified Data.List.NonEmpty as NE
import Control.Arrow
import System.Clock


doubleTapDelay :: TimeSpec
doubleTapDelay =  TimeSpec{sec = 0, nsec = 250000000}


data NoteChange =
  NoteUp Int
  | NoteDown (Int, Int)
  deriving (Show, Read, Eq)


data InputEvent =
  InDown Cell
  | InUp Cell
  | NoInput
  | InNote (NE.NonEmpty NoteChange)
  deriving (Show, Read, Eq)

makePrisms ''InputEvent

data InputState = InputState {
  _ev :: InputEvent
  ,_lastDownEv :: InputEvent
  ,_lastEv :: InputEvent
  ,_held :: [Cell]
  ,_lastTime :: TimeSpec
  ,_isDoubleTap :: Bool
  -- ,lastState :: Maybe InputState
} deriving(Show, Read, Eq)
makeLenses ''InputState

defaultInputState :: InputState
defaultInputState = InputState NoInput NoInput NoInput [] 0 False

setInputEvent :: TimeSpec -> InputEvent -> InputState -> InputState
setInputEvent curTime newEv inp = _setDoubleTap curTime $ inp{
  -- lastState = Just inp{lastState = Nothing}
  _ev = newEv
  ,_lastEv = inp^.ev

  ,_lastDownEv =
    case inp^.ev of
      InDown _ -> inp^.ev
      _ -> inp^.lastDownEv

  ,_held =
    inp^.held
    &(
      case inp^.ev of
        InDown c ->
          case newEv of
            InDown _ -> (:) c
            InNote((NoteDown _) NE.:|_) -> (:) c
            _ -> id
        InUp c -> id
        _ -> id
    )
    & (case newEv of
        NoInput -> id
        InUp c ->
            (case inp^.ev of
              InDown cell -> (:) cell
              _ -> id
            )
            >>>filter((/=) c)
        InDown _ -> id
        InNote _ -> id -- TODO: "setInputEvent: handle InNote better?"
      )
}

_setDoubleTap :: TimeSpec -> InputState -> InputState
_setDoubleTap curTime inp =
  inp
  & isDoubleTap .~
    case inp^.lastDownEv of
      InDown lastC ->
        case inp^.ev  of
          InDown c ->
            let
              same = (c == lastC)
              timeDiff = diffTimeSpec curTime (inp^.lastTime)
              withinTime = doubleTapDelay > timeDiff
            in
              same && withinTime
          _ -> False
      _ -> False
  & lastTime .~ curTime



filterDowns :: Foldable t => t Cell -> InputState -> InputState
filterDowns inps =
  held %~ filter (\x -> inps&notElem x)

filterKeys :: Foldable t => t Cell -> InputState -> InputState
filterKeys inps =
  (filterDowns inps)
  >>> (ev %~ \e -> case e of
      InDown c -> inps&find ((==) c)&maybe e (const $ NoInput)
      InUp c -> inps&find ((==) c)&maybe e (const $ NoInput)
      _ -> e
    )


keyDown :: Cell -> InputState -> Bool
keyDown k inp =
  (isJust $ inp^.held & find ((==) k))
  || (case inp^.ev of
        InDown c -> c==k
        _ -> False
    )

getDownKeys inp =
  case inp^.ev of
    InDown c -> c:inp^.held
    _ -> inp^.held

keyHeld :: Cell -> InputState -> Bool
keyHeld k inp =
  isJust $ inp^.held & find ((==) k)

keysDown :: [Cell] -> InputState -> Bool
keysDown keys inp =
  keys&all (flip keyDown inp)

keysHeld:: [Cell] -> InputState -> Bool
keysHeld keys inp =
  keys&all (flip keyHeld inp)

getDownRowPos :: InputEvent -> Maybe RowIndex
getDownRowPos = \case
    InDown (RowPos p) -> Just p
    _ -> Nothing

getUpRowPos :: InputEvent -> Maybe RowIndex
getUpRowPos = \case
    InDown (RowPos p) -> Just p
    _ -> Nothing



offsetEvent :: Int -> Int -> InputEvent -> InputEvent
offsetEvent offX offY = \case
  InDown c -> InDown (offsetCell offX offY c)
  InUp c -> InUp (offsetCell offX offY c)
  x -> x

offsetCell :: Int -> Int -> Cell -> Cell
offsetCell offX offY = \case
  GridXY x y ->
    let
      newX = x+offX
      newY = y+offY
    in
      GridXY newX newY
  x -> x


offsetInput offX offY inp = inp{
    _ev = offsetEvent offX offY (inp^.ev)
    ,_lastDownEv = offsetEvent offX offY (inp^.lastDownEv)
    ,_lastEv = offsetEvent offX offY (inp^.lastEv)
    ,_held = map (offsetCell offX offY) (inp^.held)
  }
