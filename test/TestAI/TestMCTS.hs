module TestAI.TestMCTS where

import Test.Tasty
import Test.Tasty.HUnit

import AI.MCTS

-- | Entire test suite
mctsTests :: TestTree
mctsTests = testGroup "MCTS: Tests" []

-- | all QuickCheck and SmallCheck property tests
mctsProperties :: TestTree
mctsProperties = testGroup "MCTS: Properties" []


-- | all HUnit tests
mctsHUnitTests :: TestTree
mctsHUnitTests  = testGroup "MCTS: Unit Tests" []




