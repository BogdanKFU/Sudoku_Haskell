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