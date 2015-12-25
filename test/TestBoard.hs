module TestBoard where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Monad

import ConnectFour.Board
import ConnectFour.Square

import Utils
--newtype Column = Column [Square]

--instance Arbitrary Column where
--    arbitrary = liftM Column (vector numCols)

-- | Entire test suite
boardTests :: TestTree
boardTests = testGroup "Board: Tests" [boardProperties, boardHUnitTests]

-- | all QuickCheck and SmallCheck property tests
boardProperties :: TestTree
boardProperties = testGroup "Board: Properties" []


-- | all HUnit tests
boardHUnitTests :: TestTree
boardHUnitTests  = testGroup "Board: Unit Tests" [testCheckPlayable]


-- | Tests that boards that are not full are playable.
-- | Boards that are full are not playable
testCheckPlayable = testGroup "CheckPlayable: HUnit Tests" 
    [
    testCase "check that board is playable in initial state" $
        checkPlayable initialBoard @?= True

    , testCase "check that board is not playable when full" $
        checkPlayable fullBoard @?= False

    , testCase "Check that a board is playable when almost full" $
        checkPlayable almostFullBoard @?= True
    ]
    where fullBoard = replicate numCols (replicate numRows redSquare)
          almostFullBoard = [[emptySquare] ++ replicate (numRows-1) redSquare] ++ replicate (numCols-1) (replicate numRows redSquare)




