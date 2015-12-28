module ConnectFour.Piece where

-- | A Piece can either be red or black
data Piece = RedPiece
           | BlackPiece
        deriving (Eq)

instance Show Piece where
    show RedPiece   = "R"
    show BlackPiece = "B"


-- | Constructs a new RedPiece
redPiece :: Piece
redPiece = RedPiece

-- | Construcs a new BlackPiece
blackPiece :: Piece
blackPiece = BlackPiece

-- | Returns the opposite Piece of the passed in Piece
oppositePiece :: Piece -> Piece
oppositePiece RedPiece   = BlackPiece
oppositePiece BlackPiece = RedPiece



