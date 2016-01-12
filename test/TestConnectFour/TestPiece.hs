module TestConnectFour.TestPiece where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import ConnectFour.Piece
import Arbitrary.ConnectFour

-- | Entire test suite
pieceTests :: TestTree
pieceTests = testGroup "Piece: Tests" [pieceProperties, pieceHUnitTests]

-- | all QuickCheck and SmallCheck property tests
pieceProperties :: TestTree
pieceProperties = testGroup "Piece: Properties" [
      QC.testProperty "oppositePiece /= piece" $
        propOppositePiece 
    ]

-- | Tests that the piece returned by oppositePiece is different
-- | than the original piece
propOppositePiece piece = oppositePiece piece /= piece


-- | all HUnit tests
pieceHUnitTests :: TestTree
pieceHUnitTests  = testGroup "Piece: Unit Tests" []



