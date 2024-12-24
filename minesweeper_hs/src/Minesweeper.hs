module Minesweeper
  ( Minesweeper (..),
    initGame,
    isMine,
    nearbyMines,
    makeMove,
    printBoard,
    printFinalBoard,
    won,
    flagCell
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

data Minesweeper = Minesweeper
  { height :: Int,
    width :: Int,
    mines :: Set (Int, Int),
    revealed :: Set (Int, Int),
    flagged :: Set (Int, Int)
  } deriving (Eq, Show)

initGame :: Int -> Int -> Int -> (Int, Int) -> Either String (IO Minesweeper)
initGame h w m firstMove
  | m > h * w - 1 = Left ("Too many mines! Requested: " ++ show m ++ ", Available cells: " ++ show (h * w - 1))
  | m <= 0 = Left "Number of mines must be greater than 0"
  | h <= 0 || w <= 0 = Left "Grid dimensions must be positive"
  | h * w <= 1 = Left $ "Grid size too small to place any mines."
  | otherwise = Right $ do
      let indices = [(i, j) | i <- [0 .. h - 1], j <- [0 .. w - 1], (i, j) /= firstMove]
      mineCoords <- pickRandomMines indices m
      return $ Minesweeper
        { height = h,
          width = w,
          mines = Set.fromList mineCoords,
          revealed = Set.empty,
          flagged = Set.empty
        }

pickRandomMines :: [(Int, Int)] -> Int -> IO [(Int, Int)]
pickRandomMines indices count
  | count > length indices = error "Too many mines!"
  | otherwise = go count indices []
  where
    go 0 _ acc = return acc
    go n xs acc = do
      idx <- randomRIO (0, length xs - 1)
      let chosen = xs !! idx
      go (n - 1) (take idx xs ++ drop (idx + 1) xs) (chosen : acc)

isMine :: Minesweeper -> (Int, Int) -> Bool
isMine game cell = Set.member cell (mines game)

nearbyMines :: Minesweeper -> (Int, Int) -> Int
nearbyMines game (x, y) =
    let adjacentCells = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0, inBounds (x + dx, y + dy)]
        mineCount = length (filter (isMine game) adjacentCells)
    in mineCount
  where
    inBounds (i, j) = i >= 0 && j >= 0 && i < height game && j < width game

makeMove :: Minesweeper -> (Int, Int) -> Minesweeper
makeMove game cell@(x, y)
  | isMine game cell = game {revealed = Set.insert cell (revealed game)}
  | Set.member cell (revealed game) = game 
  | otherwise =
      let game' = game {revealed = Set.insert cell (revealed game)}
          adjacentCells = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx /= 0 || dy /= 0), inBounds (x + dx, y + dy)]
          revealableCells = filter (\c -> not (Set.member c (revealed game'))) adjacentCells
      in if nearbyMines game' cell == 0
            then foldl makeMove game' revealableCells
            else game'
  where
    inBounds (i, j) = i >= 0 && j >= 0 && i < height game && j < width game

flagCell :: Minesweeper -> (Int, Int) -> Minesweeper
flagCell game cell
  | Set.member cell (revealed game) = game
  | Set.member cell (flagged game) = game {flagged = Set.delete cell (flagged game)}
  | otherwise = game {flagged = Set.insert cell (flagged game)}

won :: Minesweeper -> Bool
won game = Set.union (revealed game) (mines game) == Set.fromList [(i, j) | i <- [0 .. height game - 1], j <- [0 .. width game - 1]]

printBoard :: Minesweeper -> IO ()
printBoard game = mapM_ printRow [0 .. height game - 1]
  where
    printRow i = putStrLn (concat [cellSymbol (i, j) | j <- [0 .. width game - 1]])
    cellSymbol cell
      | Set.member cell (revealed game) =
        if isMine game cell
          then " X " 
          else
            let n = nearbyMines game cell
             in if n > 0 then " " ++ show n ++ " " else " . " 
      | Set.member cell (flagged game) = " F " 
      | otherwise = " * " 

printFinalBoard :: Minesweeper -> IO ()
printFinalBoard game = mapM_ printRow [0 .. height game - 1]
  where
    printRow i = putStrLn (concat [cellSymbol (i, j) | j <- [0 .. width game - 1]])
    cellSymbol cell
      | isMine game cell = " X " 
      | Set.member cell (revealed game) =
        let n = nearbyMines game cell
         in if n > 0 then " " ++ show n ++ " " else " . " 
      | Set.member cell (flagged game) = " F " 
      | otherwise = " * " 


