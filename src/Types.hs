module Types where

{- Модуль типов данных. Чтобы достать атрибут введите метку.
Например, для того, чтобы достать value у Cell введите value cell,
где cell - объект типа Cell -}
{- Тип Ячейка - элементарная единица поля,
принимает на вход visable, value, numbers of square -}

type Coordinate = (Int, Int)

gameSize :: Int
gameSize = 9

data Cell = Cell {
                   visible :: Bool
                 , value :: Int
                 , squares :: [Int]
                 } deriving Show

-- Setter for visible
setVisible :: Cell -> Bool -> Cell
setVisible cell visible = cell {
                                visible = visible
                               }

-- Setter for value
setValue :: Cell -> Int -> Cell
setValue cell value = cell {
                            value = value
                           }

-- Setter for squares
setSquares :: Cell -> [Int] -> Cell
setSquares cell squares = cell {
                                squares = squares
                               }

-- Тип Поле, принимает на вход массив ячеек
data Square = Square {
                       squareCells :: [Cell]
                     } deriving Show

-- Setter for squareCells
setSquareCells :: Square -> [Cell] -> Square
setSquareCells square squareCells = square {
                                            squareCells = squareCells
                                           }

-- Тип Игра, принимает на вход размер поля
data Game = Game {
                   size :: Int
                 , cells :: [[Cell]]
                 , gameSquares :: [Square]
                 } deriving Show

-- Setter for size
setSize :: Game -> Int -> Game
setSize game size = game {
                          size = size
                         }

-- Setter for cells
setCells :: Game -> [[Cell]] -> Game
setCells game cells = game {
                            cells = cells
                           }
-- Setter for gameSquares	
setGameSquares :: Game -> [Square] -> Game
setGameSquares game squares = game {
                                    gameSquares = squares
                                   }