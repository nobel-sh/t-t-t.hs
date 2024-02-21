module Game(
  getInput,
  printGame,
  handleEndGame,
  ) where

import Data.Char(digitToInt, isDigit)
import System.Exit(exitWith, ExitCode(..))
import State
import Board
import Socket
import Network.Socket


getInput :: Cell -> Socket -> IO (Int,Int)
getInput player sock = do
  putStrLn "Input the cell cordinates e.g. 1 2"
  case player of
    Cross -> inputFromStdin  sock
    Knot  -> inputFromSocket sock

inputFromStdin :: Socket -> IO (Int, Int)
inputFromStdin sock= do
  input <- getLine
  case filterInput input of
    Just a -> do
      transmitChanges a sock
      return a
    Nothing -> do
      putStrLn "invalid input. please enter two integers."
      inputFromStdin sock

-- continuously listen for input from the socket
inputFromSocket :: Socket -> IO (Int,Int)
inputFromSocket sock = do
  input <- readLineSocket sock
  case filterInput input of
    Just a -> return a
    Nothing -> do
      _ <- sendLineSocket sock "invalid input. please enter two integers."
      inputFromSocket sock


filterInput :: String -> Maybe (Int, Int)
filterInput text = case filtered text of
                     [row, col] -> Just (row,col)
                     _          -> Nothing
                where filtered :: String -> [Int]
                      filtered input = map digitToInt (filter isDigit input)



printGame :: GameState -> IO ()
printGame game = do
  putStr "\ESC[2J"              -- Clear the Screen
  putStrLn "\tGame State is:\n"
  printState game
  putStrLn "\n\n"

handleEndGame :: Cell -> IO ()
handleEndGame endingCell =
  case endingCell of
    Empty -> do
      putStrLn "No more possible moves"
      exitWith ExitSuccess
    _ -> do
      putStrLn $ "Winner: " ++ show endingCell
      exitWith ExitSuccess
