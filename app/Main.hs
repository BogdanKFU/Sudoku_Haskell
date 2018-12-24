module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Sudoku
import Types
import Data.List (transpose)

-- Это главный метод для запуска программы
main :: IO ()
main = do
    let display = InWindow "Sudoku" (screenWidth, screenHeight) (200, 200)
    let bgColor = black   -- цвет фона
    let fps     = 60      -- кол-во кадров в секунду
    playIO display bgColor fps initGame drawGame handleGame updateGame


-- =========================================
-- Модель игры
-- =========================================

-- | Фишки игроков.
data Mark = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell_UI = Maybe Mark

-- | Игровое поле.
type Board = [[Cell_UI]]

-- | Состояние игры.
data Game = Game
  { gameBoard  :: Board       -- ^ Игровое поле.
  , gamePlayer :: Mark        -- ^ Чей ход?
  , gameWinner :: Maybe Mark  -- ^ Победитель.
  }

-- | Начальное состояние игры.
initGame :: Game
initGame = Game
  { gameBoard  = replicate boardHeight (replicate boardWidth Nothing)
  , gamePlayer = One
  , gameWinner = Nothing
  }


-- =========================================
-- Отрисовка игры
-- =========================================


-- | Отобразить игровое поле.
drawGame :: Game -> IO Picture
drawGame game = do
                   let c = fromIntegral cellSize
                   let w = fromIntegral screenWidth  / 2
                   let h = fromIntegral screenHeight / 2

                   if(firstRunning (boardWidth-1) (boardHeight-1) (gameBoard game) == True) then do
                       values <- generateSudoku []
                       let game = Game (getGenerateField values) One Nothing
                       let g = translate (-w) (-h) (scale c c (pictures [ drawGrid, drawBoard (gameWinner game) (gameBoard game)]))
                       return g
				   else do
                        let g = translate (-w) (-h) (scale c c (pictures [ drawGrid, drawBoard (gameWinner game) (gameBoard game)]))
                        return g
                
             

-- | Сетка игрового поля.
drawGrid :: Picture
drawGrid = color white (pictures (hs ++ vs))
  where
    hs = map (\j -> line [(0, j), (n, j)]) [1..m - 1]
    vs = map (\i -> line [(i, 0), (i, m)]) [1..n - 1]

    n = fromIntegral boardWidth
    m = fromIntegral boardHeight

-- | Нарисовать фишки на игровом поле.
drawBoard :: Maybe Mark -> Board -> Picture
drawBoard win board = pictures (map pictures drawCells)
  where
    drawCells = map drawRow (zip [0..] board)
    drawRow (j, row) = map drawCellAt (zip [0..] row)
      where
        drawCellAt (i, cell) = translate (0.5 + i) (0.5 + j)
          (drawCell (estimate board) win cell)

-- | Нарисовать фишку в клетке поля (если она там есть).
drawCell :: (Int, Int) -> Maybe Mark -> Cell_UI -> Picture
drawCell _ _ Nothing = blank
drawCell (one, two) win (Just mark)
    = color markColor (drawMark mark)
    where
      markColor
       | win == Just mark = light orange
       | otherwise = white

-- | Нарисовать фишку.
drawMark :: Mark -> Picture
drawMark One =  translate (-0.32) (-0.25) $ scale 0.01 0.005 (text "1")
drawMark Two =  translate (-0.34) (-0.25) $ scale 0.008 0.005 (text "2")
drawMark Three = translate (-0.38) (-0.25) $ scale 0.01 0.005 (text "3")
drawMark Four = translate (-0.34) (-0.25) $ scale 0.008 0.005 (text "4")
drawMark Five = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "5")
drawMark Six = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "6")
drawMark Seven = translate (-0.38) (-0.25) $ scale 0.008 0.005 (text "7")
drawMark Eight = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "8")
drawMark Nine = translate (-0.32) (-0.25) $ scale 0.008 0.005 (text "9")

-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> (Int, Int)
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize
	
-- | Обработка событий.
handleGame :: Event -> Game -> IO Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) game = placeMark (mouseToCell mouse) game
handleGame _ w = castIO w

-- | Поставить фишку и сменить игрока (если возможно).
placeMark :: (Int, Int) -> Game -> IO Game
placeMark (i, j) game = do
    let place Nothing = Just (Just (gamePlayer game))
    let place _       = Just (Just (switchPlayer (gamePlayer game))) 

    case modifyAt j (modifyAt i place) (gameBoard game) of
      Nothing -> castIO game -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> castIO game
        { gameBoard  = newBoard
        , gamePlayer = switchPlayer (gamePlayer game)
        , gameWinner = winner newBoard
        }


-- | Сменить текущего игрока.
switchPlayer :: Mark -> Mark
switchPlayer One = Two
switchPlayer Two = Three
switchPlayer Three = Four
switchPlayer Four = Five
switchPlayer Five = Six
switchPlayer Six = Seven
switchPlayer Seven = Eight
switchPlayer Eight = Nine
switchPlayer Nine = One

-- | Определить победителя на игровом поле, если такой есть.
winner :: Board -> Maybe Mark
winner board = getFirstWinner (map lineWinner allLines)
  where
    allLines = rows ++ cols ++ diagonals
    rows = board
    cols = transpose board
    diagonals = lefts board ++ rights board

    lefts b = leftTops b ++ leftBottoms b
    rights = lefts . reverse

    leftTops    = transpose . zipWith drop [0..]
    leftBottoms = drop 1 . leftTops . transpose

    getFirstWinner :: [Maybe a] -> Maybe a
    getFirstWinner = foldr first Nothing
      where
        first Nothing y = y
        first x       _ = x

    lineWinner :: Eq a => [Maybe a] -> Maybe a
    lineWinner = winnerSegment . segments

    winnerSegment :: [(Maybe a, Int)] -> Maybe a
    winnerSegment = foldr compareSegments Nothing
      where
        compareSegments (Just x, n) _
          | n >= winnerStreak = Just x
        compareSegments _   y = y

    segments :: Eq a => [a] -> [(a, Int)]
    segments [] = []
    segments (x:xs) = segment : rest
      where
        segment = (x, 1 + length (takeWhile (== x) xs))
        rest    = segments (dropWhile (== x) xs)

-- | Оценить состояние игрового поля, а именно
-- вычислить сумму длин сегментов для крестиков и ноликов.
-- Сегменты длины 1 не учитываются при подсчёте.
estimate :: Board -> (Int, Int)
estimate _ = (0, 0)

-- | Применить преобразование к элементу списка
-- с заданным индексом. Если преобразование не удалось — вернуть 'Nothing'.
-- Иначе вернуть преобразованный список.
modifyAt :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
modifyAt _ _ []     = Nothing
modifyAt 0 f (x:xs) = case f x of
  Nothing -> Nothing
  Just y  -> Just (y : xs)
modifyAt i f (x:xs) = case modifyAt (i - 1) f xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)
	
-- | Обновление игры.
-- В этой игре все изменения происходят только по действиям игрока,
-- поэтому функция обновления — это тождественная функция.
updateGame :: Float -> Game -> IO Game
updateGame _ w= castIO w
	
-- | Сколько фишек подряд необходимо для выигрыша.
winnerStreak :: Int
winnerStreak = 4
	
boardWidth :: Int
boardWidth = 9  -- width board

boardHeight :: Int
boardHeight = 9 -- height board

-- | Размер одной клетки в пикселях.
cellSize :: Int
cellSize = 50

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth  = cellSize * boardWidth

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = cellSize * boardHeight				  


getGenerateField:: [[Cell]] -> [[Cell_UI]]
getGenerateField (x:[]) = [getLineGenerateField x]
getGenerateField (x:xs) = getLineGenerateField x : getGenerateField xs

getLineGenerateField::[Cell] -> [Cell_UI]
getLineGenerateField [] = []
getLineGenerateField (x:xs) = k : getLineGenerateField xs where 
           k | visible x == False = Nothing
             | value x == 1 = Just One
             | value x == 2 = Just Two
             | value x == 3 = Just Three
             | value x == 4 = Just Four
             | value x == 5 = Just Five
             | value x == 6 = Just Six
             | value x == 7 = Just Seven
             | value x == 8 = Just Eight
             | value x == 9 = Just Nine
			  

castIO :: Game -> IO Game
castIO game = do 
                return game

firstRunning ::Int -> Int -> Board -> Bool
firstRunning 0 0 _ =  True
firstRunning x 0 board = firstRunning (x-1) (boardHeight-1) board
firstRunning x y board = if(board !! x !! y == Nothing) then firstRunning x (y-1) board
                         else False
