module ConnectFour.Piece where

-- | A Piece can either be red or black
data Piece = RedPiece
           | BlackPiece
        deriving (Eq, Ord)

instance Show Piece where
    show RedPiece   = "R"
    show BlackPiece = "B"

pieceString :: Piece -> String
pieceString RedPiece   = "Red"
pieceString BlackPiece = "Black"

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



