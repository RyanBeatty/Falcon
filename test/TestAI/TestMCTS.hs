module TestAI.TestMCTS where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Data.Tree
import System.Random

import Utils
import AI.MCTS
import ConnectFour.GameState
import ConnectFour.Move

-- | Entire test suite
mctsTests :: TestTree
mctsTests = testGroup "MCTS: Tests" [
          QC.testProperty "possibleActions == validColumns" $
            \node -> (map column . possibleActions . expandingNode $ node) == (validColumns . state . expandingNode $ node)

        --, QC.testProperty "chooseAction `elem` possibleActions" $
        --    \tree gen -> (fst $ chooseAction (fmap expandingNode tree) (mkStdGen gen)) `elem` (possibleActions . expandingNode . rootLabel) tree 
    ]

-- | all QuickCheck and SmallCheck property tests
mctsProperties :: TestTree
mctsProperties = testGroup "MCTS: Properties" []


-- | all HUnit tests
mctsHUnitTests :: TestTree
mctsHUnitTests  = testGroup "MCTS: Unit Tests" []




