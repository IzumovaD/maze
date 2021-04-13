module Saving where

import Types
import Constants
import Data.Map
import Data.Ix (range)
import System.IO.Unsafe (unsafePerformIO)
import Numeric (lexDigits, readDec)

-- строим комнату без внутренних стен ("простой" лабиринт)
boundariesMap :: (Int, Int) -> Map Location CellBorders
boundariesMap (numColumns, numRows) = fromList
  (buildBounds <$> (range ((0, 0), (numColumns - 1, numRows - 1))))
  where
    buildBounds :: Location -> (Location, CellBorders)
    buildBounds loc =
      (loc, simpleBoundaries (numColumns, numRows) loc)
    simpleBoundaries :: (Int, Int) -> Location -> CellBorders
    simpleBoundaries (numColumns, numRows) (x, y) = CellBorders
     (if y + 1 < numRows
        then NeighbourCell (x, y + 1)
        else Border)
      (if x + 1 < numColumns
        then NeighbourCell (x + 1, y)
        else Border)
      (if y > 0 then NeighbourCell (x, y - 1) else Border)
      (if x > 0 then NeighbourCell (x - 1, y) else Border)  

-- сохраняем лабиринт в файл 
mazeToFile :: Map Location CellBorders -> IO ()
mazeToFile arr = Prelude.writeFile savingFile (myConcat (Prelude.map processing (toList arr)))
    where 
      processing :: (Location, CellBorders) -> String
      processing (loc, CellBorders up right down left) = myConcat (Prelude.map borderToSymbols [up, right, down, left])
      myConcat :: [String] -> String
      myConcat [] = []
      myConcat [x] = x
      myConcat (x:y:xs) = x ++ y ++ myConcat xs
      -- записываем в файл построчно код каждой границы каждой клетки лабиринта
      borderToSymbols :: BorderType -> String
      borderToSymbols Border = "b \n"
      borderToSymbols Wall = "w \n"
      borderToSymbols (NeighbourCell (x, y)) = (show x) ++ " " ++ (show y) ++ "\n"


-- загружаем лабиринт из файла
mazeFromFile :: Map Location CellBorders
              -- если файл пустой, создаем комнату без стен (простой лабиринт)
mazeFromFile | (length $ lines $ unsafePerformIO $ readFile savingFile) == 0 = Saving.boundariesMap (amountColumns, amountRows)
              -- иначе строим лабиринт из файла
             | otherwise = fromList (locXY 0 0 (lines $ unsafePerformIO $ readFile savingFile))
  where 
    locXY :: Int -> Int -> [String] -> [(Location, CellBorders)]
    locXY x y arr | arr == [] = []
                  | y == amountRows - 1 = formMaze arr x y : locXY (x + 1) 0 ((iterate tail arr) !! 4)
                  | otherwise = formMaze arr x y : locXY x (y + 1) ((iterate tail arr) !! 4)
    formMaze :: [String] -> Int -> Int -> (Location, CellBorders)
    formMaze arr x y = ((x, y), formBorders arr)
    formBorders :: [String] -> CellBorders   
    formBorders arr = (CellBorders (check $ head arr)   
                                   (check $ head $ tail arr)
                                   (check $ head $ tail $ tail arr)
                                   (check $ head $ tail $ tail $ tail arr)
                      ) 
    check :: String -> BorderType
    check (x:xs) | x == 'b' = Border
                 | x == 'w' = Wall 
                 | otherwise = NeighbourCell (getLoc (x:xs))
    getLoc :: String -> Location
    getLoc str = (fst $ head $ readDec $ fst $ head $ lexDigits str,
                  fst $ head $ readDec $ tail $ snd $ head $ lexDigits str)
