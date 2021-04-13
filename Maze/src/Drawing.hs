module Drawing where

import Types
import Constants
import Graphics.Gloss
import Data.Map 


-- получаем координаты клетки
locationToCoords ::
  (Float, Float) -> Float -> Location -> CellCoordinates
locationToCoords (xOffset, yOffset) cellSize (x, y) = CellCoordinates
  (centerX, centerY) -- центр
  (centerX - halfCell, centerY + halfCell) -- левый верхний угол
  (centerX + halfCell, centerY + halfCell) -- правый верхний
  (centerX - halfCell, centerY - halfCell) -- нижний левый
  (centerX + halfCell, centerY - halfCell) -- нижний правый
  where
    (centerX, centerY) =
      ( xOffset + (fromIntegral x) * cellSize
      , yOffset + (fromIntegral y) * cellSize)
    halfCell = cellSize / 2.0

-- ф-ция отрисовки мира
drawingFunc :: (Float, Float) -> Float -> World -> IO Picture
drawingFunc (xOffset, yOffset) cellSize world 
  -- меню игры после прохождения игроком лабиринта 
  | worldState world == PlayerWin = return $
      Pictures [ translate (-170) 100 $ Scale 0.15 0.25 (Text "Congratulations! You've won! Press:")
               , translate (-205) 50 $ Scale 0.15 0.25 (Text "- Enter to restart with a new maze")
               , translate (-275) 0 $ Scale 0.15 0.25 (Text "- Space to save this maze and restart with a new one")
               , translate (-205) (-50) $ Scale 0.15 0.25 (Text "- Tab to load a new maze from file")
               ]
  -- отрисовка стен во время пошаговой генерации
  | worldState world == Building1 || worldState world == Building2 = return $ mapGrid
  -- отрисовка мира во время прохождения лабиринта 
  | worldState world == InProgress = return $ Pictures [mapGrid, startPic, endPic, playerMarker]
  -- в других случаях ничего не рисуем
  | otherwise = return $ Pictures []
  where
    conversion = locationToCoords (xOffset, yOffset) cellSize
    (px, py) = cellCenter (conversion (playerLocation world))
    -- значок игрока
    playerMarker = translate px py (Circle 10)
    startCoords = conversion (startLocation world)
    endCoords = conversion (endLocation world)
    -- начало лабиринта
    startPic = Color green (Polygon
      [ cellTopLeft startCoords
      , cellTopRight startCoords
      , cellBottomRight startCoords
      , cellBottomLeft startCoords
      ])
    -- конец лабиринта
    endPic = Color green (Polygon
      [ cellTopLeft endCoords
      , cellTopRight endCoords
      , cellBottomRight endCoords
      , cellBottomLeft endCoords
      ])
    -- стены лабиринта (узкие прямоугольники)
    mapGrid = Pictures $concatMap makeWallPictures
      (toList (worldBoundaries world))
    makeWallPictures :: (Location, CellBorders) -> [Picture]
    makeWallPictures ((x,y), CellBorders up right down left) =
      let coords = conversion (x,y)
          tl@(tlx, tly) = cellTopLeft coords
          tr@(trx, try) = cellTopRight coords
          bl@(blx, bly) = cellBottomLeft coords
          br@(brx, bry) = cellBottomRight coords
      in  [ drawEdge (tr, tl, (tlx, tly - 2), (trx, try - 2)) up
          , drawEdge (br, tr, (trx - 2, try), (brx - 2, bry)) right
          , drawEdge (bl, br, (brx, bry + 2), (blx, bly + 2)) down
          , drawEdge (tl, bl, (blx + 2, bly), (tlx + 2, tly)) left
          ]
    drawEdge :: (Point, Point, Point, Point) ->
                 BorderType -> Picture
    -- если между клетками нет стены, рисуем тонкую линию между ними
    drawEdge (p1, p2, _, _) (NeighbourCell _) = Line [p1, p2]
    drawEdge (p1, p2, p3, p4) _ =
      Color red (Polygon [p1, p2, p3, p4])