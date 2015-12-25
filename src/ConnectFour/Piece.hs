module ConnectFour.Piece where


data Piece = RedPiece
           | BlackPiece
        deriving (Show, Eq)

redPiece :: Piece
redPiece = RedPiece

blackPiece :: Piece
blackPiece = BlackPiece



