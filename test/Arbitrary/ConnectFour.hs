{-# LANGUAGE FlexibleInstances #-}
module Arbitrary.ConnectFour where

import Test.Tasty.QuickCheck as QC
import Control.Monad
import Control.Applicative
import Data.List

import ConnectFour.Piece
import ConnectFour.Square
import ConnectFour.Move
import ConnectFour.Board
import ConnectFour.GameState

-- | Picks a random Piece type
instance Arbitrary Piece where
    arbitrary = elements [redPiece, blackPiece]

-- | Picks a random column
instance Arbitrary Column where
    arbitrary = elements columns

-- | Picks a move with a random column and piece type
instance Arbitrary Move where
    arbitrary = move <$> arbitrary <*> arbitrary

-- | Picks a random Square
instance Arbitrary Square where
    arbitrary = elements [emptySquare, redSquare, blackSquare]

-- | Generates a row of either empty squares or
-- | a mix of empty and filled squares
instance Arbitrary [Square] where
    arbitrary = frequency [(1, emptyRow), (7, randomRow)]
        where emptyRow = return $ replicate numRows emptySquare
              randomRow = do
                n <- choose (1,numRows) :: Gen Int
                let filled = vectorOf n $ elements [redSquare, blackSquare]
                    empty  = pure (replicate (numRows-n) emptySquare) :: Gen [Square]
                (++) <$> empty <*> filled

instance Arbitrary Board where
    arbitrary = vectorOf numCols arbitrary `suchThat` validBoard
        where validBoard board = abs (numReds - numBlacks) <= 1
                where countColor color = foldr (\row acc -> acc + foldr (\s a -> if s == color then a+1 else a) 0 row) 0 board
                      numReds          = countColor redSquare
                      numBlacks        = countColor blackSquare 



-----------------Need to refactor below-----------------



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

newtype RowWonBoard = RowWonBoard {
        rowWonBoard :: Board    
    } deriving(Show)

-- | Arbitrary generator for FilledBoard. generates a random, non-empty board
instance Arbitrary FilledBoard where
    arbitrary = liftM FilledBoard (vectorOf numCols $ genFilledList numRows)

-- | Arbitrary generator for AlmostFilledBoard. Generates a board that has a few empty squares
instance Arbitrary AlmostFilledBoard where
    arbitrary = liftM AlmostFilledBoard (generator >>= shuffle)
        where generator = liftM2 (:) genAlmostFilledColumn (vectorOf (numCols-1) $ genFilledList numRows)

-- | Arbitrary generator for ColumnWonBoard.
-- | Generates a board that will always have a column with a four-in-a-row sequence
instance Arbitrary ColumnWonBoard where
    arbitrary = liftM ColumnWonBoard (generator >>= shuffle)
        where generator = liftM2 (:) (genWonColumn numRows) (vectorOf (numCols-1) $ genEmptyList numRows)

-- | Arbitrary generator for a RowWonBoard. Generates a board that will
-- | always have a ron with a four-in-a-row sequence
instance Arbitrary RowWonBoard where
    arbitrary = liftM RowWonBoard (generator >>= shuffle >>= return . transpose) 
        where generator = liftM2 (:) (genWonColumn numCols) (vectorOf (numRows-1) $ genEmptyList numCols) 

-- | Generates a filled square
genFilledSquare :: Gen Square
genFilledSquare = oneof [return redSquare, return blackSquare]

-- | Generates an empty square
genEmptySquare :: Gen Square
genEmptySquare = oneof [return emptySquare]

-- | Generates a list of filled squares
-- | :len: The length of the generated list
genFilledList :: Int -> Gen [Square]
genFilledList len = vectorOf len genFilledSquare

-- | Generates a list of empty squares
-- | :len: The length of the generated list
genEmptyList :: Int -> Gen [Square]
genEmptyList len = vectorOf len genEmptySquare

-- | Generates a column of random squares where one random square is empty
genAlmostFilledColumn :: Gen [Square]
genAlmostFilledColumn = column >>= shuffle
    where column = liftM2 (:) (oneof [return emptySquare]) (genFilledList (numRows-1))

-- | Generates a column with a four-in-a row win sequence
genWonColumn :: Int -> Gen [Square]
genWonColumn len = do square <- genFilledSquare                          -- choose square color
                      n <- choose (0, len-4)                             -- choose random index to insert winning sequence
                      xs <- vectorOf (len-4) genEmptySquare   -- generate random column
                      let (as, bs) = splitAt n xs                        -- split the column at the index
                      return (as ++ (replicate 4 square) ++ bs)          -- insert winning sequence





