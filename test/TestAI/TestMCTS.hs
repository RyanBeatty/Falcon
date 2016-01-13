module TestAI.TestMCTS where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import System.Random
import Data.Tree
import Data.Set

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
            \tree -> (possibleActions tree) == (validMoves . state . rootLabel $ tree)

        --, QC.testProperty "choosenAction is an element of PossibleActions" $
        --    propChooseAction

        , QC.testProperty "fullyExpanded nodes have maximum length" $
            propIsFullyExpanded
    ]

-- | Tests that if a root node is fully expanded, then the length
-- | of its children is the same 
propIsFullyExpanded t1 =
    isFullyExpanded t1 ==>
        length cActions == length pActions &&
        childSet == possibleSet
    where cActions    = childrenActions t1
          pActions    = possibleActions t1
          childSet    = fromList cActions :: Set Action
          possibleSet = fromList pActions :: Set Action 

propChooseAction tree seed =
    (gamePlayable . state . rootLabel $ tree) && (not . isFullyExpanded $ tree) ==>
        (fst $ chooseAction tree gen) `elem` possibleActions tree
    where gen    = mkStdGen seed 

-- | all QuickCheck and SmallCheck property tests
mctsProperties :: TestTree
mctsProperties = testGroup "MCTS: Properties" []


-- | all HUnit tests
mctsHUnitTests :: TestTree
mctsHUnitTests  = testGroup "MCTS: Unit Tests" []




