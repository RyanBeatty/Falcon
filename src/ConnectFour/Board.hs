module ConnectFour.Board where

import ConnectFour.Square
import ConnectFour.Move
import ConnectFour.Utils

import Data.List


-- | Represents a connect4 board. A Board is a list of list of Squares
type Board = [[Square]]

-- | Represents the possible states the board can be in.
-- | BoardPlayable: A player can make a move
-- | BoardDraw: The Board is full
-- | BoardWon: A Player has gotten four-in-a-row
data BoardState = BoardPlayable
                | BoardDraw
                | BoardWon
                deriving (Show, Eq)

boardPlayable = BoardPlayable
boardDraw = BoardDraw
boardWon = BoardWon

showBoard :: Board -> String
showBoard = unlines . (['1'..'7'] :) . map concat . map (map show) . transpose
                  
-- | Number of Rows in the Board
numRows :: Int
numRows = 6

-- | Number of Columns in the Board
numCols :: Int
numCols = 7

-- | Initial Board state. An initial Board is just empty
initialBoard :: Board
initialBoard = replicate numCols (replicate numRows emptySquare)




---------- Functions for Updating the Board ----------

-- | Checks if a move can be made in a specified Column on the Board.
-- | A Move can be made if the first element in the Column is an empty Square
-- | This is because pieces are added to Columns right to left
canMove :: Column -> Board -> Bool
canMove column board = board !! col !! 0 == emptySquare
    where col = fromEnum column
    
-- | Places a Square in the next empty slot in a Column.
-- | NOTE: throws error if the column is full
placeSquareInColumn :: Square -> [Square] -> [Square]
placeSquareInColumn square column = replaceNth insertIndex square column
    where insertIndex = (length . takeWhile (==emptySquare) $ column) - 1

placeSquareInBoard :: Square -> Column -> Board -> Board
placeSquareInBoard square column board = replaceNth colIndex newCol board
    where colIndex = fromEnum column
          col      = board !! colIndex
          newCol   = placeSquareInColumn square col


updateBoard :: Move -> Board -> Maybe Board
updateBoard move board 
    | canMove col board = Just (placeSquareInBoard newSquare col board)
    | otherwise         = Nothing 
    where newPiece  = piece move
          newSquare = square (Just newPiece)
          col       = column move 



---------- Functions for Checking the BoardState ----------

-- | checks if a color has gotten four-in-a-row in any columns
checkWonColumns :: Square -> Board -> Bool
checkWonColumns square = any (checkColumn square)
    where checkColumn square = isInfixOf (replicate 4 square)

-- | to check the rows for a winner, transpose the matrix
-- | so the rows are now columns, and call checkRows
checkWonRows :: Square -> Board -> Bool
checkWonRows square = checkWonColumns square . transpose

-- | Gets all of the diagonals of the board and checks for a winner
checkWonDiagonals :: Square -> Board -> Bool
checkWonDiagonals square = checkWonColumns square . allDiagonals

-- | Checks if a player has gotten four-in-a-row on the board
checkWon :: Board -> Bool
checkWon board = or 
    [ checkWonColumns redSquare board
    , checkWonColumns blackSquare board
    , checkWonRows redSquare board
    , checkWonRows blackSquare board
    , checkWonDiagonals redSquare board
    , checkWonDiagonals blackSquare board
    ]

-- | Checks that the game can be continued to be played given
-- | the state of the board
checkPlayable :: Board -> Bool
checkPlayable = or . map (checkColumn)
    where checkColumn = elem emptySquare  

-- | Returns the current state of the Board
-- | checks if a player has won, if a move can be played
-- | or if it is a draw
getBoardState :: Board -> BoardState
getBoardState board
    | checkWon board       = boardWon
    | checkPlayable  board = boardPlayable
    | otherwise            = boardDraw

-- | Returns a list of the empty columns on the board
emptyColumns :: Board -> [Column]
emptyColumns board = filter (flip canMove board) columns

