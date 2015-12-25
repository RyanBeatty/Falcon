module ConnectFour.Square where

import ConnectFour.Piece

newtype Square = Square (Maybe Piece)
	deriving (Show, Eq)

emptySquare :: Square
emptySquare = Square Nothing

redSquare :: Square
redSquare = Square (Just RedPiece)

blackSquare :: Square
blackSquare = Square (Just BlackPiece)



