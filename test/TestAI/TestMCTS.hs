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

        , QC.testProperty "choosenAction is an element of PossibleActions" $
            propChooseAction

        , QC.testProperty "fullyExpanded nodes have chosen all possible actions" $
            propIsFullyExpanded
    ]

-- | Tests that if a root node is fully expanded, then the 
-- | node has chosen all possible actions
propIsFullyExpanded t1 t2 =
    isFullyExpanded t1 && (not . isFullyExpanded $ t2) ==>
        test t1 && (not . test $ t2)
    where test t = length cActions == length pActions &&
                   childSet == possibleSet
            where cActions    = childrenActions t
                  pActions    = possibleActions t
                  childSet    = fromList cActions :: Set Action
                  possibleSet = fromList pActions :: Set Action 

-- | Tests that chooseAction returns an action that is a possible action
-- | and that has not been chosen by yet
propChooseAction tree seed =
    (gamePlayable . state . rootLabel $ tree) && (not . isFullyExpanded $ tree) ==>
        action `elem` possibleActions tree &&
        action `notElem` childrenActions tree
    where gen    = mkStdGen seed
          action =  fst $ chooseAction tree gen

-- | all QuickCheck and SmallCheck property tests
mctsProperties :: TestTree
mctsProperties = testGroup "MCTS: Properties" []


-- | all HUnit tests
mctsHUnitTests :: TestTree
mctsHUnitTests  = testGroup "MCTS: Unit Tests" []




