module ConnectFour.Piece where


data Piece = RedPiece
           | BlackPiece
        deriving (Eq)

redPiece :: Piece
redPiece = RedPiece

blackPiece :: Piece
blackPiece = BlackPiece



