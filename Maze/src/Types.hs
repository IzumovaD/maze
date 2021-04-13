module Types where

import Data.Map 
import Graphics.Gloss
import System.Random

-- местоположение клетки в лабиринте - (номер столбца, номер ряда)
type Location = (Int, Int)

-- состояние мира
-- Building1 - построение простого лабиринта (комната без стен)
-- Building2 - пошаговая генерация лабиринта (внутренних стен)
-- Generating - новая генерация лабиринта (после выбора пользователя)
-- InProgress - прохождение лабиринта
-- PlayerWin - победа игрока (лабиринт успешно пройден)
data WorldState = Building1 | Building2 | Generating | InProgress | PlayerWin 
  deriving (Show, Eq)

-- тип границы клетки 
-- Border - внешняя граница (крайний ряд/столбец)
-- Wall - внутренняя стена 
-- NeighbourCell Location - местоположение соседней клетки (стены нет)
data BorderType = Border | Wall | NeighbourCell Location
  deriving (Show, Eq)

-- описание всех четырёх границ клетки 
data CellBorders = CellBorders
  { upBoundary :: BorderType      -- верхняя
  , rightBoundary :: BorderType   -- правая
  , downBoundary :: BorderType    -- нижняя
  , leftBoundary :: BorderType    -- левая
  }

-- дерево построения лабиринта
data RoomTree a = Leaf a a a a          -- комната, которая больше не делится
                  | Node a a a a        -- комната, которую можно разделить на 4 маленьких
                    (RoomTree a)        -- левая верхняя подкомната
                    (RoomTree a)        -- левая нижняя подкомната
                    (RoomTree a)        -- правая верхняя подкомната
                    (RoomTree a)        -- правая нижняя подкомната

-- мир 
data World = World
  { playerLocation :: Location                      -- позиция игрока
  , startLocation :: Location                       -- начало лабиринта
  , endLocation :: Location                         -- конец лабиринта
  , worldState :: WorldState                        -- состояние мира
  , innerRooms :: (RoomTree Int)                    -- текущее состояние дерева построения лабиринта
  , worldBoundaries :: Map Location CellBorders     -- массив из элементов типа (местоположение клетки, её границы)
  , worldGen :: StdGen                              -- генератор случайных чисел
  , save :: Bool                                    -- флаг сохранения лабиринта
  }

-- координаты клетки
data CellCoordinates = CellCoordinates
  { cellCenter :: Point          -- центр
  , cellTopLeft :: Point         -- верхний левый угол
  , cellTopRight :: Point        -- верхний правый угол
  , cellBottomLeft :: Point      -- нижний левый угол
  , cellBottomRight :: Point     -- нижний правый угол
  }