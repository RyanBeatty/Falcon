module ConnectFour.Board where

import ConnectFour.Square

type Board = [[Square]]

numRows :: Int
numRows = 6

numCols :: Int
numCols = 7

initialBoard :: Board
initialBoard = replicate numCols [replicate numRows emptySquare]




