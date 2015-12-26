module ConnectFour.Square where

import ConnectFour.Piece

newtype Square = Square (Maybe Piece)
	deriving (Show, Eq)

square :: Maybe Piece -> Square
square = Square

emptySquare :: Square
emptySquare = Square Nothing

redSquare :: Square
redSquare = Square (Just RedPiece)

blackSquare :: Square
blackSquare = Square (Just BlackPiece)



