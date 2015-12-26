module ConnectFour.Move where

import ConnectFour.Piece

-- | Enumerates the Columns of the Board
data Column = One
			| Two
			| Three
			| Four
			| Five
			| Six
			| Seven
		deriving (Show, Enum)


data Move = Move 
	{ piece  :: Piece
	, column :: Column
	}




