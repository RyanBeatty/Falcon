module TestAI.TestMCTS where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Arbitrary.MCTS
import ConnectFour
import AI.MCTS

-- | Entire test suite
mctsTests :: TestTree
mctsTests = testGroup "MCTS: Tests" [
          --QC.testProperty "possibleActions == validColumns" $
          --  \node -> (map column . possibleActions . expandingNode $ node) == (validColumns . state . expandingNode $ node)

        --, QC.testProperty "chooseAction `elem` possibleActions" $
        --    \tree gen -> (fst $ chooseAction (fmap expandingNode tree) (mkStdGen gen)) `elem` (possibleActions . expandingNode . rootLabel) tree 

          QC.testProperty "visitCount updatedNode > originalNode" $
            \node reward -> (visitCount . updateNode reward $ node) == visitCount node + 1

        , QC.testProperty "possibleActions == validMoves" $
            \node -> (possibleActions $ node) == (validMoves . state $ node)
    ]

-- | all QuickCheck and SmallCheck property tests
mctsProperties :: TestTree
mctsProperties = testGroup "MCTS: Properties" []


-- | all HUnit tests
mctsHUnitTests :: TestTree
mctsHUnitTests  = testGroup "MCTS: Unit Tests" []




