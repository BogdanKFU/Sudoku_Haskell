module Sudoku where

import Types

-- Генерация ячеек
generateCells :: Int -> [[Cell]]
generateCells size = undefined

-- Генерация областей, в которых каждый value встречается ровно один раз
generateSquares :: Int -> Int -> [Square]
generateSquares = undefined

generateSudoku :: [Char] -> IO()
generateSudoku size = do
                        putStrLn ("Generation is in progress")
                        {- Запускается генерация ячеек -}
                        {- Отображение сгенерированной игры -}



--Генерация поля, в качестве параметра передается размер поля
generateValues:: Int -> [[Cell]]
generateValues 0 = []
generateValues size = (defineLineValues gameSize $take gameSize $drop ((gameSize-size)*3+ div (gameSize-size) 3) $ cycle [1..gameSize]) : generateValues(size - 1) 

--вспомогательная функция для generateValues, принимает размер поля и лист данных из которого генерировать ячейки
defineLineValues:: Int -> [Int] -> [Cell]
defineLineValues 0 _ = []
defineLineValues x xs = defineLineValues (x-1) xs ++ [Cell True (xs !! (x-1)) []]

--преобразование двумерного листа cell в лист их значений
getListValuesFromCell:: [[Cell]] -> [[Int]]
getListValuesFromCell (x:[])= [getLineValuesFromCell x]
getListValuesFromCell (x:xs) = getLineValuesFromCell x : getListValuesFromCell xs

--вспомогательная функция для getLineValuesFromCell, принимает лист cells и преобразует его в лист интов
getLineValuesFromCell :: [Cell] -> [Int]
getLineValuesFromCell [] = []
getLineValuesFromCell (cell:xs) = value cell : getLineValuesFromCell xs