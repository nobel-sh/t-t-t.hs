{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Socket
import State
import Board
import Network.Socket
import Game

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

clientMain :: IO ()
clientMain = do
  sock <- initClient 4000
  let game = initializeState emptyBoard Cross
  runClientGame game sock

hostMain ::  IO ()
hostMain = do
  sock <- initServer 4000
  let game = initializeState emptyBoard Cross
  runHostGame game sock

main = hostMain
