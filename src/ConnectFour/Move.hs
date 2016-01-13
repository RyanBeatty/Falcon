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
        deriving (Show, Enum, Eq, Ord)

columns :: [Column]
columns = [(One)..(Seven)]

readColumn :: String -> Maybe Column
readColumn "1" = Just One
readColumn "2" = Just Two
readColumn "3" = Just Three
readColumn "4" = Just Four
readColumn "5" = Just Five
readColumn "6" = Just Six
readColumn "7" = Just Seven
readColumn _   = Nothing

-- | A Move has a Piece to place and
-- | Column number to place the piece
data Move = Move 
    { column :: Column
    , piece  :: Piece
    } deriving (Show, Eq, Ord)

move :: Column -> Piece -> Move
move = Move 




