module Test.Piece where

import Test.Tasty
import Test.HUnit

import ConnectFour.Piece

-- | Entire test suite
pieceTests :: TestTree
pieceTests = testGroup "Piece: Tests" [pieceProperties, pieceHUnitTests]

-- | all QuickCheck and SmallCheck property tests
pieceProperties :: TestTree
pieceProperties = testGroup "Piece: Properties" []


-- | all HUnit tests
pieceHUnitTests :: TestTree
pieceHUnitTests  = testGroup "Piece: Unit Tests" [testOppositePiece]

-- | Test cases for oppositePiece
testOppositePiece = testGroup "oppositePiece: HUnit Tests" $
	[ testCase "oppositePiece Red == Black" $
		oppositePiece redPiece @?= blackPiece

	, testCase "oppositePiece Black == Red" $
		oppositePiece blackPiece @?= redPiece
	]





