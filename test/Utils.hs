module Utils where

import Test.Tasty.QuickCheck as QC


import ConnectFour.Piece
import ConnectFour.Square


instance Arbitrary Piece where
	arbitrary = oneof [return redPiece, return blackPiece]

instance Arbitrary Square where
	arbitrary = oneof [return emptySquare, return redSquare, return blackSquare]







