module TestBoard where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import ConnectFour.Board


-- | Entire test suite
boardTests :: TestTree
boardTests = testGroup "Board: Tests" [boardProperties, boardUnitTests]

-- | all QuickCheck and SmallCheck property tests
boardProperties :: TestTree
boardProperties = testGroup "Board: Properties" []


-- | all HUnit tests
boardUnitTests :: TestTree
boardUnitTests  = testGroup "Board: Unit Tests" []




