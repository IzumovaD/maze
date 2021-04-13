module Handler where

import Types
import Generating
import Saving
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Map 
import Data.Maybe (fromJust)

-- ф-ция обработки действий пользователя
inputHandler :: Event -> World -> IO World
inputHandler event w 
  -- выбор из меню после прохождения лабиринта
  | worldState w == PlayerWin = return $ case event of
    -- пошаговая генерация нового лабиринта по нажатию клавиши Enter
    (EventKey (SpecialKey KeyEnter) Down _ _) -> w { save = False, worldState = Generating, worldBoundaries = fromList [] }
    -- сохранение текущего лабиринта и пошаговая генерация нового по нажатию пробела
    (EventKey (SpecialKey KeySpace) Down _ _) -> w { save = True, worldState = Generating }
    -- загрузка лабиринта из файла по нажатию клавиши Tab
    (EventKey (SpecialKey KeyTab) Down _ _) -> w { playerLocation = (0, 0)
                                                 , worldState = InProgress
                                                 , worldBoundaries = mazeFromFile 
                                                 }
    _ -> w
  -- прохождение лабиринта (используются клавиши вверх-вниз, вправо-влево)
  | worldState w == InProgress = return $ case event of
    (EventKey (SpecialKey KeyUp) Down _ _) ->
      w { playerLocation = nextLocation upBoundary }
    (EventKey (SpecialKey KeyDown) Down _ _) ->
      w { playerLocation = nextLocation downBoundary }
    (EventKey (SpecialKey KeyRight) Down _ _) ->
      w { playerLocation = nextLocation rightBoundary }
    (EventKey (SpecialKey KeyLeft) Down _ _) ->
      w { playerLocation = nextLocation leftBoundary }
    _ -> w
  -- в остальных случаях управление блокируется
  | otherwise = return $ w
  where
    cellBounds = fromJust $ Data.Map.lookup (playerLocation w) (worldBoundaries w)
    nextLocation :: (CellBorders -> BorderType) -> Location
    nextLocation boundaryFunc = case boundaryFunc cellBounds of
      (NeighbourCell cell) -> cell
      _ -> playerLocation w 