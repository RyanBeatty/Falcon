module TestUtils where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import ConnectFour.Utils

-- | Entire test suite
utilsTests :: TestTree
utilsTests = testGroup "Utils: Tests" [utilsProperties, utilsHUnitTests]

-- | all QuickCheck and SmallCheck property tests
utilsProperties :: TestTree
utilsProperties = testGroup "Utils: Properties" []


-- | all HUnit tests
utilsHUnitTests :: TestTree
utilsHUnitTests  = testGroup "Utils: Unit Tests" [testAllDiagonals]

-- | Tests that allDiagonals will get all of the diagonals for a 3x3 matrix
testAllDiagonals = testCase "test getting diagonals on 3x3 matrix" $
        allDiagonals [[1, 2, 3], [4, 5, 6], [7, 8, 9]] @?= mainDiags ++ antiDiags
    where mainDiags = [[1], [4, 2], [7, 5, 3], [8, 6], [9]]
          antiDiags = [[3], [6, 2], [9, 5, 1], [8, 4], [7]]


