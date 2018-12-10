module Sudoku where

import System.Random
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
--Пример запроса getListValuesFromCell $ generateValues 9
getListValuesFromCell:: [[Cell]] -> [[Int]]
getListValuesFromCell (x:[])= [getLineValuesFromCell x]
getListValuesFromCell (x:xs) = getLineValuesFromCell x : getListValuesFromCell xs

--вспомогательная функция для getLineValuesFromCell, принимает лист cells и преобразует его в лист интов
getLineValuesFromCell :: [Cell] -> [Int]
getLineValuesFromCell [] = []
getLineValuesFromCell (cell:xs) = value cell : getLineValuesFromCell xs


--Алгоритмы для перемешивания значений двумерного листа

--Транспонирование матрицы
transpose:: [[a]]-> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

--Поменять две строки в пределах ее района
swapRows:: [[a]] -> IO [[a]]
swapRows xs =do
                   district <- getRandom 1 3
                   x <-getRandom 1 3
                   y <- getRandom 1 3
                   let randI = (x *district) -1
                   let randJ = (y *district) -1
                   let elemI = xs !! randI
                   let elemJ = xs !! randJ
                   let left = take randI xs
                   let middle = take(randJ - randI - 1) (drop(randI + 1) xs)
                   let right = drop (randJ + 1) xs
                   let mas = left ++ [elemJ] ++ middle ++ [elemI] ++ right
                   return mas
                   
		
swapColumns:: [[a]] ->IO [[a]]
swapColumns xs = do 
                   let transposes = transpose xs
                   swaps <- swapRows transposes
                   return (transpose swaps)

--поменять местами два района по строкам
--swapRowsArea:: [[a]] -> IO [[a]]
swapRowsArea xs = do
                    district1Num <- getRandom 1 3
                    district2Num <- getRandom 1 3
                    let district1 = getTripleFromArea 3 district1Num xs
                    let district2 = getTripleFromArea 3 district1Num xs
                    let left = take district1Num xs
                    let middle = take(district2Num - district1Num - 1) (drop(district1Num +1) xs)
                    let right = drop (district2Num +1) xs
                    let mas = left ++ district2 ++ middle ++ district2 ++ right
                    return mas
                    
--count - количество элементов в Area, в нашем случае это 3
getTripleFromArea:: Int -> Int -> [[a]] -> [[a]]
getTripleFromArea 0 _ _ = []
getTripleFromArea count numberDistrict xs = elem : getTripleFromArea (count-1) numberDistrict xs where
                                              elem = xs !! ((numberDistrict*3)-count)

					

getRandom:: Int -> Int -> IO Int
getRandom begin end = do
         g <- newStdGen
         let a = fst (randomR (begin, end :: Int) g)
         return a


			
		 
