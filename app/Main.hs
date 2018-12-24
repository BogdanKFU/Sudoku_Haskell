module Main where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Sudoku
import Data.List (transpose)
import Data.Foldable

-- Это главный метод для запуска программы
main :: IO ()
main = do
  play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Крестики-нолики" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- =========================================
-- Модель игры
-- =========================================

-- | Фишки игроков.
data Mark = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Show)

-- | Клетка игрового поля.
type Cell = Maybe Mark

-- | Игровое поле.
type Board = [[Cell]]

-- | Состояние игры.
data Game = Game
  { gameBoard  :: Board       -- ^ Игровое поле.
  , gamePlayer :: Mark        -- ^ Чей ход?
  , gameWinner :: Maybe Mark  -- ^ Победитель.
  }

-- | Начальное состояние игры.
-- Игровое поле — пусто.
-- Первый игрок ходит за крестики.
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
drawGame :: Game -> Picture
drawGame game = translate (-w) (-h) (scale c c (pictures
  [ drawGrid
  , drawBoard (gameWinner game) (gameBoard game)
  ]))
  where
    c = fromIntegral cellSize
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

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
drawCell :: (Int, Int) -> Maybe Mark -> Cell -> Picture
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
drawOne :: Picture
drawOne = pictures
  [ color ballColor $ text "asdasdas"
  ]
  where
    ballColor = dark red 

drawTwo :: Picture
drawTwo = rectangleSolid 0.1 0.5

drawThree :: Picture
drawThree = pictures
  [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
  , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]

drawFour :: Picture
drawFour = pictures
  [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
  , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]

drawFive :: Picture
drawFive = pictures
  [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
  , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]

drawSix :: Picture
drawSix = pictures
  [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
  , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]

drawSeven :: Picture
drawSeven = pictures
  [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
  , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]

drawEight :: Picture
drawEight = thickCircle 0.3 0.1

drawNine :: Picture
drawNine = pictures
  [ polygon [(-0.4,  0.3), (-0.3,  0.4), ( 0.4, -0.3), ( 0.3, -0.4)]
  , polygon [(-0.4, -0.3), (-0.3, -0.4), ( 0.4,  0.3), ( 0.3,  0.4)] ]

-- | Получить координаты клетки под мышкой.
mouseToCell :: Point -> (Int, Int)
mouseToCell (x, y) = (i, j)
  where
    i = floor (x + fromIntegral screenWidth  / 2) `div` cellSize
    j = floor (y + fromIntegral screenHeight / 2) `div` cellSize
	
-- | Обработка событий.
handleGame :: Event -> Game -> Game
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) = placeMark (mouseToCell mouse)
handleGame _ = id

-- | Поставить фишку и сменить игрока (если возможно).
placeMark :: (Int, Int) -> Game -> Game
placeMark (i, j) game =
  case gameWinner game of
    Just _ -> game    -- если есть победитель, то поставить фишку нельзя
    Nothing -> case modifyAt j (modifyAt i place) (gameBoard game) of
      Nothing -> game -- если поставить фишку нельзя, ничего не изменится
      Just newBoard -> game
        { gameBoard  = newBoard
        , gamePlayer = switchPlayer (gamePlayer game)
        , gameWinner = winner newBoard
        }
  where
    place Nothing = Just (Just (gamePlayer game))
    place _       = Nothing -- если клетка занята, поставить фишку нельзя 
	
-- | Сменить текущего игрока.
switchPlayer :: Mark -> Mark
switchPlayer One = Two
switchPlayer Two = One

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
updateGame :: Float -> Game -> Game
updateGame _ = id
	
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