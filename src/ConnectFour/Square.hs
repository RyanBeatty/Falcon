module ConnectFour.Square where

import ConnectFour.Piece

-- | A Square can either have a Piece or be empty
newtype Square = Square {
		squarePiece :: Maybe Piece
	}
    deriving (Eq)

instance Show Square where
    show (Square Nothing)  = "."
    show (Square (Just p)) = show p

-- | Constructs a new non-empty Square from the passed in Piece
square :: Maybe Piece -> Square
square = Square

-- | Constructs an empty Square
emptySquare :: Square
emptySquare = Square Nothing

-- | Constructs a Square filled with a RedPiece
redSquare :: Square
redSquare = Square (Just RedPiece)

-- | Constructs a Square filled with a BlackPiece
blackSquare :: Square
blackSquare = Square (Just BlackPiece)



