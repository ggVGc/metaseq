{-# LANGUAGE RankNTypes #-}
module GenericModes where

import Control.Lens
import SeqTypes
import ControllerCommon
import AppMode
import Input
import ControllerFunctions


gridSelectMode :: Enum n => Traversal' SeqModel n -> Color -> AppMode
gridSelectMode lns color = nopMode{
  _update = simpleUpdate $ \_ input m funcs ->
    case input^.ev of
      InDown cell@(GridXY _ _) ->
        Just $ m&lns .~ toEnum ((funcs&indexFromCell) cell)
      _ -> Nothing

  ,_render = \_ inp m funcs ->
    m^?lns&maybe [] (\v ->
      [ ((funcs&gridFromIndex) (fromEnum v), color)]
    )
}
