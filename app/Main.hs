{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Socket
import State
import Board
import Network.Socket
import Game
import Data.List (find)
import System.Environment

runHostGame :: GameState -> Socket -> IO ()
runHostGame game sock= do
  printGame game
  case isEndState game of
    Just endCell -> handleEndGame endCell
    Nothing      -> do
      inputs  <- getInput (currTurn game) sock
      let newGame = applyModification game inputs
      runHostGame newGame sock

runClientGame :: GameState -> Socket -> IO ()
runClientGame game sock= do
  printGame game
  case isEndState game of
    Just endCell -> handleEndGame endCell
    Nothing      -> do
      inputs <- getInput (nextTurn $ currTurn game) sock
      let newGame = applyModification game inputs
      runClientGame newGame sock

clientMain :: String -> IO ()
clientMain port = do
  sock <- initClient port
  let game = initializeState emptyBoard Cross
  runClientGame game sock

hostMain :: String -> IO ()
hostMain port = do
  sock <- initServer port
  let game = initializeState emptyBoard Cross
  runHostGame game sock

isHost ::  [String] -> Bool
isHost args = "-H" `elem` args

getPortArg :: [String] -> Maybe String
getPortArg args = findPortArg args
  where
    findPortArg = fmap snd . find ((== "-P") . fst) . zip args . tail

main :: IO ()
main = do
    args <- getArgs
    case getPortArg args of
      Nothing -> do
        putStrLn "Please enter a valid port"
      Just p  -> if isHost args then hostMain p else clientMain p
