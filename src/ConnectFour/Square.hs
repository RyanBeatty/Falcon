module ConnectFour.Square where

import ConnectFour.Piece

type Square = Maybe Piece

emptySquare :: Square
emptySquare = Nothing

redSquare :: Square
redSquare = Just RedPiece

blackSquare :: Square
blackSquare = Just BlackPiece



