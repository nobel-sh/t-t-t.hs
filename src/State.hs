module State
    (
      GameState(currBoard, currTurn),
      initializeState,
      printState,
      modifyState,
      applyModification,
      isEndState,
      nextTurn,
    ) where
import Board

data GameState = GameState
  {
   currBoard :: Board
  ,currTurn :: Cell
  } deriving (Eq, Show)

initializeState :: Board -> Cell -> GameState
initializeState board cell=  GameState{
        currBoard = board
       ,currTurn = cell
      }

nextTurn :: Cell -> Cell
nextTurn s = case s of
               Cross -> Knot
               Knot  -> Cross
               _     -> Empty

modifyState :: GameState -> Int -> Int -> Maybe GameState
modifyState state row col
  | row < 1 || row > 3  || col < 1 || col > 3 = Nothing
  | not $ isCellEmpty (board !! row' !! col' ) = Nothing
  | otherwise =
      let (before, notChanged:after) = splitAt row' $ board -- row to change
          changed = take col' notChanged
                    ++ [nextTurn $ turn]
                    ++ drop col notChanged
      in Just GameState {
         currBoard = before ++ [changed] ++ after
        ,currTurn  = nextTurn $ currTurn state
        }
  where row'  = row -1
        col'  = col - 1
        board = currBoard state
        turn  = currTurn state

isEndState :: GameState -> Maybe Cell
isEndState game
  | isBoardFull currGame = Just (Empty)
  | otherwise = hasWinner currGame
  where currGame = currBoard game

applyModification :: GameState -> (Int, Int) -> GameState
applyModification state (row, col) =
  case modifyState state row col of
        Nothing -> state
        Just a -> a

printPlayer :: Cell -> IO ()
printPlayer p = do
        putStr $ "\tTURN: " ++ show (nextTurn p) ++ "\n"

printState :: GameState -> IO ()
printState g = do
    printPlayer $ currTurn g
    printBoard  $ currBoard g
    putStrLn "\n"
