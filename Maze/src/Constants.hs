module Constants where

import Graphics.Gloss.Interface.IO.Game

--число симуляционных шагов в секунду
stepPerSec :: Int
stepPerSec = 5

-- фоновый цвет
bgColor :: Color
bgColor = white

-- размер клетки
worldCellSize :: Float
worldCellSize = 25

-- смещение по координате X
worldXOffset :: Float
worldXOffset = -300

-- смещение по координате Y
worldYOffset :: Float
worldYOffset = -300

-- число столбцов в лабиринте
amountColumns :: Int
amountColumns  = 24

-- число рядов в лабиринте
amountRows :: Int
amountRows  = 24

-- файл для сохранения и загрузки
savingFile :: FilePath
savingFile = "SavedMaze.txt" 

-- режим окна 
windowDisplay :: Display
windowDisplay = InWindow "Maze" (625, 625) (10, 10)