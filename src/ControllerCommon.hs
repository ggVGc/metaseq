{-# LANGUAGE LambdaCase #-}
module ControllerCommon  where

import ZMidi.Core

data MessageType =
  Note
  | CC
  deriving (Show, Read, Eq)

data RowIndex =
  Top Int
  | Side Int
  deriving (Show, Read, Eq, Ord)

data Cell =
  GridXY Int Int
  | RowPos RowIndex
  | Shift
  -- | Shift2
  deriving (Show, Read, Eq, Ord)



data Color =
  NoCol

  | StrongRed
  | MediumRed
  | WeakRed

  | StrongGreen
  | MediumGreen
  | WeakGreen

  | StrongAmber
  | MediumAmber
  | WeakAmber

  | R1G2
  | R1G3

  | R2G1
  | R2G3

  | R3G1
  | R3G2
  deriving (Eq, Show)

makeWeakCol = \case
  StrongRed -> WeakRed
  StrongGreen -> WeakGreen
  StrongAmber -> WeakAmber
  x -> x

getRedGreenFromColor :: Color -> (Int, Int)
getRedGreenFromColor c =
  case c of
    NoCol -> (0, 0)

    StrongRed -> (3, 0)
    MediumRed -> (2, 0)
    WeakRed -> (1, 0)

    StrongGreen -> (0, 3)
    MediumGreen -> (0, 2)
    WeakGreen -> (0, 1)

    StrongAmber -> (3, 3)
    MediumAmber -> (2, 2)
    WeakAmber -> (1, 1)

    R1G2 -> (1, 2)
    R1G3 -> (1, 3)

    R2G1 -> (2, 1)
    R2G3 -> (2, 3)

    R3G1 -> (3, 1)
    R3G2 -> (3, 2)


data State = State {
    topRow :: [Color]
    ,sideRow :: [Color]
    ,grid :: [[Color]]
  }


createState :: Color -> State
createState initCol = State {
    topRow = replicate 8 initCol
    ,sideRow = replicate 8 initCol
    ,grid = replicate 8 (replicate 8 initCol)
  }


makeChannelMessage :: (Integral a, Integral a1) => a -> a1 -> MessageType -> MidiEvent
makeChannelMessage key vel messageType =
    VoiceEvent RS_OFF $ c 0 (fromIntegral key) (fromIntegral vel)
    where
      c = case messageType of
        Note -> NoteOn
        CC -> Controller


