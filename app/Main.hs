module Main (main) where

import Data.Char(digitToInt, isDigit)
import System.Exit(exitWith, ExitCode(..))
import State
import Board

getInput :: IO (Int,Int)
getInput = do
  putStrLn "Input the cell cordinates e.g. 1 2"
  input <- getLine
  let cordinates = map digitToInt (filter isDigit input)
  case cordinates of
    [row, col] -> return (row,col)
    _          -> do
        putStrLn "Invalid Input. Please enter two integers."
        getInput

printGame :: GameState -> IO ()
printGame game = do
  putStr "\ESC[2J"              -- Clear the Screen
  putStrLn "\tGame State is:\n"
  printState game
  putStrLn "\n\n"

runGame :: GameState -> IO ()
runGame game = do
  printGame game
  case isEndState game of
    Just endCell -> handleEndGame endCell
    Nothing      -> do
      inputs <- getInput
      runGame $ applyModification game inputs

handleEndGame :: Cell -> IO ()
handleEndGame endingCell =
  case endingCell of
    Empty -> do
      putStrLn "No more possible moves"
      exitWith ExitSuccess
    _ -> do
      putStrLn $ "Winner: " ++ show endingCell
      exitWith ExitSuccess

main ::  IO ()
main = do
  runGame $ initializeState emptyBoard Cross
