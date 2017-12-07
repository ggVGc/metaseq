-- {-# OPTIONS -Wall #-}

module Main where
import Player

-- import qualified System.Console.ANSI as ANSI
-- import MainLinux
import MainPortMidi
import Data.Vector.Storable


main :: IO ()
-- main = mainLinux
main = mainPortMidi



