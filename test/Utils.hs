module Utils where

import Test.Tasty.QuickCheck as QC
import Control.Monad

import ConnectFour.Piece
import ConnectFour.Square
import ConnectFour.Board

-- | Represents a board that has no empty spaces
newtype FilledBoard = FilledBoard Board
    deriving (Show)

-- | Arbitrary instance for Pieces. Either a RedPiece or BlackPiece is generated
instance Arbitrary Piece where
    arbitrary = oneof [return redPiece, return blackPiece]

-- | Arbitrary generator for Squares. Either emptysquares, redsquares, or blacksquares are returned
instance Arbitrary Square where
    arbitrary = oneof [return emptySquare, return redSquare, return blackSquare]

-- | Arbitrary generator for FilledBoard. generates a random, non-empty board
instance Arbitrary FilledBoard where
    arbitrary = liftM FilledBoard (vectorOf numRows genFilledColumn)

-- | Generates a filled square
genFilledSquare :: Gen Square
genFilledSquare = oneof [return redSquare, return blackSquare]

-- | Generates a column of filled squares
genFilledColumn :: Gen [Square]
genFilledColumn = vectorOf numCols genFilledSquare


genAlmostFilledColumn :: Gen [Square]
genAlmostFilledColumn = column >>= shuffle
    where column = liftM2 (:) (oneof [return emptySquare]) (vectorOf (numCols-1) genFilledSquare)







