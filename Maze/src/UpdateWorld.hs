module UpdateWorld where

import Types
import Constants
import Saving
import Generating
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- ф-ция обновления дерева 
-- (Node (-2) (-2) (-2) (-2)) - пройденный узел 
-- (Leaf (-2) (-2) (-2) (-2))  - пройденный лист
updateTree :: [Int] -> (RoomTree Int) -> (RoomTree Int) 
-- спускаемся вглубь дерева влево, насколько это возможно
updateTree arr (Node (-2) (-2) (-2) (-2) one two three four)
  | takeBorders one == (-2, -2, -2, -2) && takeBorders two == (-2, -2, -2, -2) && takeBorders three == (-2, -2, -2, -2) =
      (Node (-2) (-2) (-2) (-2) one two three (updateTree arr four))
  | takeBorders one == (-2, -2, -2, -2) && takeBorders two == (-2, -2, -2, -2) =  
      (Node (-2) (-2) (-2) (-2) one two (updateTree arr three) four)
  | takeBorders one == (-2, -2, -2, -2) = 
      (Node (-2) (-2) (-2) (-2) one (updateTree arr two) three four)
  | otherwise = (Node (-2) (-2) (-2) (-2) (updateTree arr one) two three four)
-- текущую комнату больше нельзя поделить, помечаем её пройденным листом
updateTree [] (Leaf _ _ _ _) = (Leaf (-2) (-2) (-2) (-2)) 
-- делим текущую комнату на 4 маленькие и помечаем её как пройденный узел
updateTree arr (Leaf _ _ _ _) = (Node (-2) (-2) (-2) (-2) 
                                      (Leaf (head arr) (takeElem arr 1) (takeElem arr 2) (takeElem arr 3))
                                      (Leaf (takeElem arr 4) (takeElem arr 5) (takeElem arr 6) (takeElem arr 7))
                                      (Leaf (takeElem arr 8) (takeElem arr 9) (takeElem arr 10) (takeElem arr 11))
                                      (Leaf (takeElem arr 12) (takeElem arr 13) (takeElem arr 14) (takeElem arr 15))
                                )
  where 
    -- выделяем из массива границ нужную
    takeElem :: [Int] -> Int -> Int
    takeElem x num = head $ (iterate tail x) !! num 

-- ф-ция взятия границ очередной комнаты
takeBorders :: (RoomTree Int) -> (Int, Int, Int, Int)
-- спускаемся вглубь дерева влево, насколько это возможно
takeBorders (Node (-2) (-2) (-2) (-2) one two three four)
  | takeBorders one == (-2, -2, -2, -2) && takeBorders two == (-2, -2, -2, -2) && takeBorders three == (-2, -2, -2, -2) = takeBorders four
  | takeBorders one == (-2, -2, -2, -2) && takeBorders two == (-2, -2, -2, -2) = takeBorders three
  | takeBorders one == (-2, -2, -2, -2) = takeBorders two
  | otherwise = takeBorders one
-- как только добрались до самого левого непройденного листа, возвращаем границы соответствующей комнаты
takeBorders (Leaf a b c d) = (a, b, c, d)

-- ф-ция обновления мира
updateFunc :: Float -> World -> IO World
updateFunc _ w
  -- строим комнату без стен (простейший лабиринт) 
  | worldState w == Building1 = return $ w { worldState = Building2 
                                           , innerRooms = (Leaf 0 (amountColumns - 1) 0 (amountRows - 1))  
                                           , worldBoundaries = Generating.boundariesMap (amountColumns, amountRows)
                                           }
  -- лабиринт полностью сгенерирован, передаём управление игроку
  | worldState w == Building2 && takeBorders (innerRooms w) == (-2, -2, -2, -2) = return $ w { worldState = InProgress }
  -- пошаговая генерация внутренних стен
  | worldState w == Building2 =
    let (newMaze, borders, newGen) = Generating.buildBorders (worldBoundaries w) (takeBorders (innerRooms w)) (worldGen w)
    in return $ w { worldState = Building2 
                  , innerRooms = updateTree borders (innerRooms w)   -- обновляем дерево построения лабиринта
                  , worldBoundaries = newMaze
                  , worldGen = newGen
                  }
  -- сохранение текущего лабиринта и построение нового
  | save w == True && worldState w == Generating = do
    let saving = worldBoundaries w
    mazeToFile saving
    return $ w { worldState = Building1
               , playerLocation = (0, 0)
               , save = False 
               }
  -- генерация нового лабиринта без сохранения текущего
  | worldState w == Generating = return $ w { worldState = Building1
                                            , playerLocation = (0, 0)
                                            , save = False 
                                            }
  -- лабиринт успешно пройден
  | playerLocation w == endLocation w = return $ w { worldState = PlayerWin }
  | otherwise = return w
