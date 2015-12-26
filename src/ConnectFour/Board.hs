module ConnectFour.Board where

import ConnectFour.Square
import ConnectFour.Move
import ConnectFour.Utils

import Data.List

type Board = [[Square]]



data BoardState = Playable
                | Draw
                | Won
                  

numRows :: Int
numRows = 6

numCols :: Int
numCols = 7

initialBoard :: Board
initialBoard = replicate numCols (replicate numRows emptySquare)




--canMove :: Board -> Column -> Bool
--canMove board col = board !! c !! 0 == emptySquare
--    where c = fromEnum col

--updateBoard :: Board -> Column -> Maybe Board
--updateBoard board col
--    | not canMove board col = Nothing
--    | otherwise             =

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
-- | TODO: test
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
-- | TODO: test
checkPlayable :: Board -> Bool
checkPlayable = or . map (checkColumn)
    where checkColumn = elem emptySquare  

-- | Returns the current state of the Board
-- | checks if a player has won, if a move can be played
-- | or if it is a draw
-- | TODO: test
getBoardState :: Board -> BoardState
getBoardState board
    | checkWon board       = Won
    | checkPlayable  board = Playable
    | otherwise            = Draw

