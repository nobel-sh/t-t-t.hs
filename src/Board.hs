module Board(
        Board,
        Cell(Cross, Knot, Empty),
        emptyBoard,
        printBoard,
        isCellEmpty,
        isBoardFull,
        hasWinner,
        ) where

data Cell =
  Cross
  | Knot
  | Empty
  deriving (Eq,Show)

type Row   = [Cell]
type Board = [Row]

emptyRow :: Row
emptyRow = [Empty, Empty, Empty]

emptyBoard :: Board
emptyBoard = [emptyRow, emptyRow, emptyRow]

isCellEmpty :: Cell -> Bool
isCellEmpty cell = case cell of
                     Empty -> True
                     _     -> False

isBoardFull :: Board -> Bool
isBoardFull b = (all isRowFull b)
                && (hasWinner b == Nothing)
                where isRowFull :: Row -> Bool
                      isRowFull r = all (/=Empty) r

horizontal :: Board -> Cell -> Bool
horizontal b c = any (rowSameElems) b
                 where rowSameElems :: Row -> Bool
                       rowSameElems r = all (==c) r


vertical :: Board -> Cell -> Bool
vertical b c = horizontal (transpose b) c
               where transpose :: [[a]] -> [[a]]
                     transpose ([]:_) = []
                     transpose board =
                       (map head board)
                       : transpose (map tail board)

diagonal :: Board -> Row
diagonal b =  [b !! i !! i | i <-[0..2]]

-- anti diagonal
adiagonal :: Board -> Row
adiagonal b = [b !! i !!(2-i) | i<-[0, 1, 2]]


-- checks for both diagonal and antidiagonal
diagonals :: Board -> Cell -> Bool
diagonals b c
    | all (== head diagonal') diagonal'
      && head diagonal' == c              = True
    | all (== head adiagonal') adiagonal'
      && head adiagonal' == c             = True
    | otherwise                           = False
    where diagonal'  = diagonal b
          adiagonal' = adiagonal b

hasWinner :: Board -> Maybe (Cell)
hasWinner board
  | checkWinningDirs Cross = Just (Cross)
  | checkWinningDirs Knot  = Just (Knot)
  | otherwise              = Nothing
  where
    checkWinningDirs :: Cell -> Bool
    checkWinningDirs cell =
      (horizontal   board cell)
      || (vertical  board cell)
      || (diagonals board cell)

printCell :: Cell -> IO ()
printCell cell = case cell of
  Empty -> putStr " _ "
  Knot  -> putStr " O "
  Cross -> putStr " X "

printBoard :: Board -> IO ()
printBoard board = mapM_ printRow board
                    where
                      printRow :: Row -> IO ()
                      printRow row = do
                          putStr "\t\t"
                          mapM_ printCell row
                          putStrLn "\n"
