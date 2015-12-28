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

-- | A Move has a Piece to place and
-- | Column number to place the piece
data Move = Move 
    { column :: Column
    , piece  :: Piece
    }

move :: Column -> Piece -> Move
move = Move 




