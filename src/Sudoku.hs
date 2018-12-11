module Sudoku where

import System.Random
import Types
import Generation

-- Генерация ячеек
generateCells :: Int -> [[Cell]]
generateCells size = undefined

generateSudoku :: [Char] -> IO()
generateSudoku size = do
                        putStrLn ("Generation is in progress")
                        {- Запускается генерация ячеек -}
                        {- Отображение сгенерированной игры -}

--вычисление решаемости
checkPossibleToSolve :: [[Cell]] -> Bool
checkPossibleToSolve cells = True

--вычисление самих решений
getSudokuSolutions :: [[Cell]] -> Int
getSudokuSolutions cells = undefined

--выкалывание ячеек
gougeOut :: IO [[Cell]] -> [[Int]] -> Int -> IO [[Cell]]
gougeOut cells flook iterator = do if iterator >= (gameSize ^ 2)
                                      then do
                                              c <- cells
                                              return c
                                      else do
                                              i <- getRandom 0 (gameSize - 1)
                                              j <- getRandom 0 (gameSize - 1)
                                              if (flook !! i) !! j == 0
                                                 then
                                                      do
                                                         cellsNotIO <- cells
                                                         let temp = (cellsNotIO !! i) !! j
                                                         let flook = replaceElementForDoubleList flook i j 1
                                                         let cell = setVisible temp False
                                                         let cells = replaceElementForDoubleList cells i j cell
                                                         --if checkPossibleToSolve cells
                                                            --then
                                                                 --let cells = replaceElementForDoubleList cells i j temp
                                                         gougeOut (pure cells) flook (iterator + 1)
                                                 else gougeOut cells flook iterator


--заменяет элемент в двумерном списке
replaceElementForDoubleList :: [[a]] -> Int -> Int -> a -> [[a]]
replaceElementForDoubleList list index j arg = if (index < length list - 1) && (index /= 0)
                                        then (take index list) ++ ((replaceElement (list !! index) j arg) : drop (index + 1) list)
                                        else
                                            if (index == 0)
									           then (replaceElement (list !! index) j arg) : (drop 1 list)
                                               else (take (index - 1) list) ++ [(replaceElement (list !! index) j arg)]
--заменяет элемент в списке
replaceElement :: [a] -> Int -> a -> [a]
replaceElement list index arg = if (index < length list - 1) && (index /= 0)
                                        then (take index list) ++ (arg : drop (index + 1) list)
                                        else
                                            if (index == 0)
									           then arg : (drop 1 list)
                                               else (take (index - 1) list) ++ [arg]

--заполнить массив непосещенных ячеек
generateFlook :: Int -> [[Int]]
generateFlook 0 = []
generateFlook size = (generateFlookLines gameSize $take gameSize $drop ((gameSize-size)*3 + div (gameSize - size) 3) $ cycle [1..gameSize]) : generateFlook(size - 1)

generateFlookLines :: Int -> [Int] -> [Int]
generateFlookLines 0 _ = []
generateFlookLines x xs = generateFlookLines (x-1) xs ++ [0]