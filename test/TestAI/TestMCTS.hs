module TestAI.TestMCTS where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Utils
import AI.MCTS
import ConnectFour.GameState
import ConnectFour.Move

-- | Entire test suite
mctsTests :: TestTree
mctsTests = testGroup "MCTS: Tests" [
          QC.testProperty "possibleActions == validColumns" $
            \node -> (map column . possibleActions . expandingNode $ node) == (validColumns . state . expandingNode $ node)
    ]

-- | all QuickCheck and SmallCheck property tests
mctsProperties :: TestTree
mctsProperties = testGroup "MCTS: Properties" []


-- | all HUnit tests
mctsHUnitTests :: TestTree
mctsHUnitTests  = testGroup "MCTS: Unit Tests" []




