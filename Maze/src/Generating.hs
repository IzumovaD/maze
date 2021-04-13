module Generating where

import Types
import Constants
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Array
import Data.Ix (range)
import Data.Map 
import Data.Maybe (fromJust)
import System.Random

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

-- map для функций buildingVert и buildingHorizont с пятью аргументами
mapFiveArgs :: (a -> Int -> Int -> Int -> Int -> a) -> [a] -> Int -> Int -> Int -> Int -> [a]
mapFiveArgs f [] _ _ _ _ = []
mapFiveArgs f (x:xs) a b c d = f x a b c d : mapFiveArgs f xs a b c d

-- строим внутренние стены в комнате, используя алгоритм рекурсивного деления
buildBorders :: Map Location CellBorders -> (Int, Int, Int, Int) -> StdGen ->
                    (Map Location CellBorders, [Int], StdGen)
buildBorders arr (firstColumn, lastColumn, firstRow, lastRow) r
    -- продолжение рекурсии
  | lastColumn - firstColumn >= 1 && lastRow - firstRow >= 1 = 
    let (maze, bordersArr, gen) = recursionCon (toList arr) firstColumn lastColumn firstRow lastRow r
    in (fromList $ correction $ maze, bordersArr, gen)
    -- конец рекурсии 
  | otherwise = (arr, [], r) 
  where
    recursionCon :: [(Location, CellBorders)] -> Int -> Int -> Int -> Int -> StdGen -> 
                      ([(Location, CellBorders)], [Int], StdGen)
    recursionCon arr firstColumn lastColumn firstRow lastRow r = 
          -- выбираем случайным образом абсциссу вертикальной стены в промежутке (firstColumn, lastColumn)
      let (vertWall, newGen1) = randomR (firstColumn, lastColumn - 1) r          
          -- ординату прохода в вертикальной стене в промежутке (firstRow, lastRow)
          (vertPass, newGen2) = randomR (firstRow, lastRow) newGen1      
          -- ординату горизонтальной левой стены в промежутке (firstRow, lastRow)
          (leftHorWall, newGen3) = randomR (firstRow + 1, lastRow) newGen2 
          -- абсциссу прохода в горизонтальной левой стене в промежутке (firstColumn, vertWall)
          (leftHorPass, newGen4) = randomR (firstColumn, vertWall) newGen3 
          -- ординату горизонтальной правой стены в промежутке (firstRow, lastRow) 
          (rightHorWall, newGen5) = randomR (firstRow + 1, lastRow) newGen4  
          -- абсциссу прохода в горизонтальной правой стене  в промежутке (vertWall, lastColumn)
          (rightHorPass, newGen6) = randomR (vertWall + 1, lastColumn) newGen4        
          -- строим вертикальную стену
          vertLine = mapFiveArgs buildingVert arr firstRow lastRow vertWall vertPass  
          -- строим горизонтальную стену слева от вертикальной
          horLeftLine = mapFiveArgs buildingHorizont vertLine firstColumn vertWall leftHorWall leftHorPass  
          -- строим горизонтальную стену справа от вертикальной
          horRightLine = mapFiveArgs buildingHorizont horLeftLine (vertWall + 1) lastColumn rightHorWall rightHorPass
      -- возвращаем обновлённый лабиринт, массив границ для очередных четырёх внутренних комнат, новый генератор 
      in (horRightLine, [ firstColumn, vertWall, leftHorWall, lastRow
                            , firstColumn, vertWall, firstRow, (leftHorWall - 1)
                            , (vertWall + 1), lastColumn, rightHorWall, lastRow
                            , (vertWall + 1), lastColumn, firstRow, (rightHorWall - 1)
                            ], newGen6)
    -- ф-ция построения вертикальной стены с проходом
    buildingVert :: (Location, CellBorders) -> Int -> Int -> Int -> Int -> (Location, CellBorders) 
    buildingVert ((x, y), CellBorders up right down left) inf sup wall pass
      | (sup - inf) < 1 = ((x, y), CellBorders up right down left)
      | (y >= inf) && (y <= sup) && (y /= pass) && (x == wall) = ((x, y), CellBorders up Wall down left)
      | otherwise = ((x, y), CellBorders up right down left)
    -- ф-ция построения горизонтальной стены с проходом
    buildingHorizont :: (Location, CellBorders) -> Int -> Int ->  Int -> Int -> (Location, CellBorders)      
    buildingHorizont ((x, y), CellBorders up right down left) inf sup wall pass
      | (sup - inf) < 1 = ((x, y), CellBorders up right down left)
      | (x >= inf) && (x <= sup) && (x /= pass) && (y == wall) = ((x, y), CellBorders up right Wall left)
      | otherwise = ((x, y), CellBorders up right down left) 

-- map для функции correction с двумя аргументами
mapForCorrection :: ((Location, CellBorders) -> (Location, CellBorders) -> (Location, CellBorders)) 
                -> [(Location, CellBorders)] -> (Location, CellBorders) ->  [(Location, CellBorders)]
mapForCorrection f [] _ = []
mapForCorrection f (x:xs) a  = f x a : mapForCorrection f xs a            

-- корректируем верхнюю и левую границы клеток (т.к. при построении стен изменяли состояние только правой и нижней границ)
correction :: [(Location, CellBorders)] -> [(Location, CellBorders)] 
correction arr = fix arr arr
  where 
    fix :: [(Location, CellBorders)] -> [(Location, CellBorders)] -> [(Location, CellBorders)]
    fix [] (n:ns) = (n:ns)
    fix (z:zs) (n:ns) = fix zs (mapForCorrection comprasion (n:ns) z)
    comprasion :: (Location, CellBorders) -> (Location, CellBorders) -> (Location, CellBorders)
    comprasion ((x1, y1), CellBorders up1 right1 down1 left1) ((x2, y2), CellBorders up2 right2 down2 left2) 
      -- не являются соседними, нельзя сравнивать
      | abs (x1 - x2) > 1 || abs (y1 - y2) > 1 || y1 /= y2 && x1 /= x2 =
         ((x1, y1), CellBorders up1 right1 down1 left1)
      -- клетка находится в первом столбце и нижняя граница клетки над ней является стеной
      | x1 == 0 && down2 == Wall && y2 - 1 == y1 = ((x1, y1), CellBorders Wall right1 down1 left1)
      -- т.к. слева клеток нет, остальные клетки в первом столбце не меняем
      | x1 == 0 = ((x1, y1), CellBorders up1 right1 down1 left1)
      -- клетка находится в посленем ряду и правая граница клетки слева является стеной
      | y1 == (amountRows - 1) && right2 == Wall && x2 + 1 == x1 =  ((x1, y1), CellBorders up1 right1 down1 Wall)
      -- т.к. сверху клеток нет, остальные клетки в посленем ряду не меняем
      | y1 == (amountRows - 1) = ((x1, y1), CellBorders up1 right1 down1 left1)
      -- нижняя граница клетки сверху является стеной и правая граница клетки слева является стеной
      | down2 == Wall && y2 - 1 == y1 && right2 == Wall && x2 + 1 == x1 =
          ((x1, y1), CellBorders Wall right1 down1 Wall)
      -- нижняя граница клетки сверху является стеной
      | down2 == Wall && y2 - 1 == y1 = ((x1, y1), CellBorders Wall right1 down1 left1)
      -- правая граница клетки слева является стеной
      | right2 == Wall && x2 + 1 == x1 = ((x1, y1), CellBorders up1 right1 down1 Wall)
      -- остальные клетки не меняем
      | otherwise = ((x1, y1), CellBorders up1 right1 down1 left1)