module Main where

import Sudoku

-- Это главный метод для запуска программы
main :: IO ()
main = do
         putStrLn "Enter number of size sudoku game (default - 9):"
         size <- getLine
         let names = [size]
         putStrLn ("Chosen size of sudoku: " ++ size)
         generateSudoku size
{-
Тут происходит генерация поля судоку и отображение на экране
-}