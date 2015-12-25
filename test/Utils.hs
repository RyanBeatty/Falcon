module Utils where

import Test.Tasty.QuickCheck as QC
import Control.Monad

import ConnectFour.Piece
import ConnectFour.Square
import ConnectFour.Board

-- | Represents a board that has no empty spaces
newtype FilledBoard = FilledBoard {
        filledBoard :: Board
    } deriving(Show)

newtype AlmostFilledBoard = AlmostFilledBoard {
        almostFilledBoard :: Board
    } deriving(Show)

newtype ColumnWonBoard = ColumnWonBoard {
        columnWonBoard :: Board
    } deriving(Show)

-- | Arbitrary instance for Pieces. Either a RedPiece or BlackPiece is generated
instance Arbitrary Piece where
    arbitrary = oneof [return redPiece, return blackPiece]

-- | Arbitrary generator for Squares. Either emptysquares, redsquares, or blacksquares are returned
instance Arbitrary Square where
    arbitrary = oneof [return emptySquare, return redSquare, return blackSquare]

-- | Arbitrary generator for FilledBoard. generates a random, non-empty board
instance Arbitrary FilledBoard where
    arbitrary = liftM FilledBoard (vectorOf numCols genFilledColumn)

-- | Arbitrary generator for AlmostFilledBoard. Generates a board that has a few empty squares
instance Arbitrary AlmostFilledBoard where
    arbitrary = liftM AlmostFilledBoard (generator >>= shuffle)
        where generator = liftM2 (:) genAlmostFilledColumn (vectorOf (numCols-1) genFilledColumn)

--instance Arbitrary ColumnWonBoard where
--    arbitrary = liftM ColumnWonBoard (generator >>= shuffle)
--        where generator = liftM2 (:) (genFilledSquare >>= genWinColumn) (vectorOf (numRows-1) (arbitrary :: Gen [Square]))

-- | Generates a filled square
genFilledSquare :: Gen Square
genFilledSquare = oneof [return redSquare, return blackSquare]

-- | Generates a column of filled squares
genFilledColumn :: Gen [Square]
genFilledColumn = vectorOf numRows genFilledSquare

-- | Generates a column of random squares where one random square is empty
genAlmostFilledColumn :: Gen [Square]
genAlmostFilledColumn = column >>= shuffle
    where column = liftM2 (:) (oneof [return emptySquare]) (vectorOf (numRows-1) genFilledSquare)

--genWinColumn :: Square -> Gen [Square]
--genWinColumn square = column square >>= shuffle
--    where column square = liftM2 (++) (oneof [return [square, square, square, square]]) (vectorOf (numCols-1) (arbitrary :: Gen Square)) 

--genWinColumn :: Square -> Gen [Square]
--genWinColumn square = do n <- choose (0, numCols-4)






