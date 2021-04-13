module Main where

import Types
import Constants
import Generating
import Drawing
import Handler
import Data.Map
import Graphics.Gloss.Interface.IO.Game
import System.Random
import UpdateWorld

main :: IO ()
main = do
  randomGen <- newStdGen
  playIO
    windowDisplay
    bgColor
    stepPerSec
    (World (0, 0)
           (0, 0) 
           (amountColumns - 1, amountRows - 1) 
           Generating 
           (Leaf 0 (amountColumns - 1) 0 (amountRows - 1))
           (fromList []) 
           randomGen 
           False)
    (drawingFunc (worldXOffset, worldYOffset) worldCellSize)
    inputHandler
    updateFunc
