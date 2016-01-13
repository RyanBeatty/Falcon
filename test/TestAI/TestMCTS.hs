module TestAI.TestMCTS where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import System.Random
import Data.Tree
import Data.Tree.Zipper
import Data.Set

import Arbitrary.MCTS
import ConnectFour
import AI.MCTS

-- | Entire test suite
mctsTests :: TestTree
mctsTests = testGroup "MCTS: Tests" [
          QC.testProperty "visitCount updatedNode > originalNode" $
            \node reward -> (visitCount . updateNode reward $ node) == visitCount node + 1

        --, QC.testProperty "children with higher values are more valuable" $
        --    propChildValue

        , QC.testProperty "possibleActions == validMoves" $
            \tree -> (possibleActions tree) == (validMoves . state . rootLabel $ tree)

        , QC.testProperty "choosenAction is an element of PossibleActions" $
            propChooseAction

        , QC.testProperty "fullyExpanded nodes have chosen all possible actions" $
            propIsFullyExpanded

        , QC.testProperty "the action of focused node after expand should be same as chooseAction" $
            propExpand
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

propChildValue weight valParent n1 n2 =
    weight > 0 && value n1 > value n2 && visitCount n1 == visitCount n2 ==>
        childValue weight valParent n1 > childValue weight valParent n2 

-- | Tests that chooseAction returns an action that is a possible action
-- | and that has not been chosen by yet
propChooseAction tree gen =
    (gamePlayable . state . rootLabel $ tree) && (not . isFullyExpanded $ tree) ==>
        action `elem` possibleActions tree &&
        action `notElem` childrenActions tree
    where action =  fst $ chooseAction tree gen

-- | Tests that the resulting zipper after expanding contains
-- | the same action as returned by chooseAction
propExpand zipper gen =
    (not . isFullyExpanded $ tree') ==>
    chosen == (action . rootLabel . tree $ expanded)
    where tree'    = tree zipper
          expanded = fst $ expand zipper gen
          chosen   = fst $ chooseAction tree' gen

-- | all QuickCheck and SmallCheck property tests
mctsProperties :: TestTree
mctsProperties = testGroup "MCTS: Properties" []


-- | all HUnit tests
mctsHUnitTests :: TestTree
mctsHUnitTests  = testGroup "MCTS: Unit Tests" []




