module ConnectFour.Square where

import ConnectFour.Piece

newtype Square = Square (Maybe Piece)
	deriving (Eq)

emptySquare :: Square
emptySquare = Square Nothing

redSquare :: Square
redSquare = Square (Just RedPiece)

blackSquare :: Square
blackSquare = Square (Just BlackPiece)



