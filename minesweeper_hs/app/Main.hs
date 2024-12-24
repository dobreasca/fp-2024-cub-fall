module Main where

import qualified Data.Set as Set
import Minesweeper (Minesweeper, initGame, makeMove, printBoard, printFinalBoard, won, isMine, flagCell)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

defaultHeight, defaultWidth, defaultMines :: Int
defaultHeight = 8
defaultWidth = 8
defaultMines = 8

printRules :: IO ()
printRules = do
  putStrLn "Welcome to Minesweeper!"
  putStrLn "Rules and How to Play:"
  putStrLn "1. The board contains hidden mines and safe cells."
  putStrLn "2. Your goal is to reveal all safe cells without hitting a mine."
  putStrLn "3. On each turn, you can either reveal a cell, flag/unflag a cell, or quit the game:"
  putStrLn "   - To reveal a cell, enter 'R row column'."
  putStrLn "   - To flag or unflag a cell, enter 'F row column'."
  putStrLn "   - To quit the game, enter 'Q'."
  putStrLn "   Example: 'R 0 1' reveals the cell at row 0, column 1."
  putStrLn "            'F 3 2' flags the cell at row 3, column 2."
  putStrLn "4. If you reveal a mine, you lose!"
  putStrLn "5. Numbers indicate how many mines are adjacent to a cell."
  putStrLn "6. Use logic to avoid mines, flag them, and win the game!"
  putStrLn ""
  putStrLn "Board Characters and Their Meanings:"
  putStrLn "  * : Unrevealed cell."
  putStrLn "  . : Revealed safe cell with no adjacent mines."
  putStrLn "  X : Revealed mine (you lose if you see this!)."
  putStrLn "  F : Flagged cell (you think there's a mine here)."
  putStrLn "  1-8 : Revealed safe cell with the number of adjacent mines."
  putStrLn ""

main :: IO ()
main = do
  printRules
  putStrLn "Enter board dimensions (height width), or press Enter for default (8x8):"
  putStr "> "
  hFlush stdout
  input <- getLine
  let (h, w) = case parseGridSize input of
                 Just (h', w') -> (h', w')
                 Nothing -> (defaultHeight, defaultWidth)
  let numMines = computeMines h w
  putStrLn ("Starting a game with a grid of size " ++ show h ++ "x" ++ show w ++ " and " ++ show numMines ++ " mines.")
  gameLoop Nothing (h, w, numMines)

gameLoop :: Maybe Minesweeper -> (Int, Int, Int) -> IO ()
gameLoop Nothing (h, w, m) = do
  putStrLn "Enter your first move (e.g., 'R row column' or 'Q' to quit):"
  putStr "> "
  hFlush stdout
  input <- getLine
  case parseInput input of
    Just (Left firstMove) -> case initGame h w m firstMove of
      Left errMsg -> do
        putStrLn ("Error initializing game: " ++ errMsg)
        gameLoop Nothing (h, w, m)
      Right action -> do
        game <- action
        gameLoop (Just (makeMove game firstMove)) (h, w, m)
    _ -> if input == "Q"
         then putStrLn "Goodbye!"
         else do
           putStrLn "Invalid input. Please try again."
           gameLoop Nothing (h, w, m)
gameLoop (Just game) dims = do
  Minesweeper.printBoard game
  if Minesweeper.won game
    then putStrLn "Congratulations, you WON!!!"
    else do
      putStrLn "Enter your move (e.g., 'R row column', 'F row column', or 'Q' to quit):"
      putStr "> "
      hFlush stdout
      input <- getLine
      case parseInput input of
        Nothing ->
          if input == "Q"
            then putStrLn "Goodbye!"
            else do
              putStrLn "Invalid input. Please try again."
              gameLoop (Just game) dims
        Just (Left cell) ->
          if Minesweeper.isMine game cell
            then do
              putStrLn "YOU EXPLODED! GAME OVER!!!"
              Minesweeper.printFinalBoard game
            else gameLoop (Just (makeMove game cell)) dims
        Just (Right cell) -> gameLoop (Just (flagCell game cell)) dims

parseGridSize :: String -> Maybe (Int, Int)
parseGridSize input =
  case map readMaybe (words input) of
    [Just h, Just w] -> Just (h, w)
    _ -> Nothing

computeMines :: Int -> Int -> Int
computeMines h w = max 1 ((h * w) `div` 6)

parseInput :: String -> Maybe (Either (Int, Int) (Int, Int))
parseInput input =
  case words input of
    ("Q" : _) -> Nothing 
    ("R" : x : y : _) -> Just (Left (read x, read y))
    ("F" : x : y : _) -> Just (Right (read x, read y))
    _ -> Nothing 
