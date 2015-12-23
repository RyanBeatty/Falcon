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

-- | Checks if a player has gotten four-in-a-row on the board
-- | TODO: test
checkWon :: Board -> Bool
checkWon board = or [checkColumns redSquare board, checkColumns blackSquare board, checkRows redSquare board, checkRows blackSquare board, checkDiagonals redSquare board, checkDiagonals blackSquare board]
    where 
          -- | Checks if a color has gotten four-in-a-row in a row
          checkRow color = isInfixOf (replicate 4 color)

          -- | checks if a color has gotten four-in-a-row in any row
          checkRows color = or . map (checkRow color)

          -- | to check the columns for a winner, transpose the matrix
          -- | so the columns are now rows, and call checkRows
          checkColumns color = checkRows color . transpose

          -- | Gets all of the diagonals of the board and checks for a winner
          checkDiagonals color = checkRows color . allDiagonals

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

