module MCTS where

import ConnectFour.GameState (GameState)
import ConnectFour.Move (Move, Column)

import Data.Tree
import Data.Tree.Zipper
import Data.List
import System.Random

type Action = Column

data SearchNode = TerminalNode 
                { state :: GameState
                }
                | SearchNode 
                { value      :: Int
                , visitCount :: Int
                , action     :: Action
                , state      :: GameState
                }



type SearchTree = Tree SearchNode

gameNode :: Int -> Int -> Action -> GameState -> SearchNode
gameNode = undefined 

newGameNode :: Action -> GameState -> SearchNode
newGameNode action oldState = gameNode 0 0 action oldState

mctsSearch :: StdGen -> TreePos Full SearchNode -> (StdGen, TreePos Full SearchNode)
mctsSearch = (,) . backUp . defaultPolicy . treePolicy

------------------methods implementing treePolicy------------------

-- | Search faze of a single iteration of MCTS. Decends down the tree
-- | deciding until it finds either a terminal node or a node that is not
-- | fully expanded.
treePolicy :: StdGen -> TreePos Full SearchNode -> (TreePos Full SearchNode, StdGen)
treePolicy gen searchTree
  -- If a node is a TerminalNode, then stop search
  | isTerminal tree'      = (searchTree, gen)

  -- If a Node is FullyExpanded, then continue search with its best child
  | isFullyExpanded tree' = treePolicy gen bChild

  -- otherwise, expand the current node and stop search
  | otherwise             = expand searchTree gen

  where tree'            = tree searchTree
        
        -- Gets the index for the best child of the current node
        bcIndex          = bestChildIndex $ tree searchTree
        
        -- Gets the best child of the current node
        bChild           = case childAt bcIndex searchTree of
                               (Just child) -> child

        (action, newGen) = chooseAction tree' gen
        newNode          = newGameNode action (state . rootLabel $ tree')    

isTerminal :: SearchTree -> Bool
isTerminal = undefined

isFullyExpanded :: SearchTree -> Bool
isFullyExpanded = undefined

-- | Returns the best child node of a root node.
-- | The best child is the child with the highest win value
bestChild :: SearchTree -> SearchTree
--bestChild = maximum . subForest
bestChild = undefined

-- | Returns the index of the best child node
bestChildIndex :: SearchTree -> Int
--bestChildIndex = elemIndex . bestChild
bestChildIndex = undefined



------------------Methods implementing expand------------------


expand :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
expand searchTree gen = undefined

chooseAction :: SearchTree -> StdGen -> (Action, StdGen)
chooseAction searchTree gen = undefined


------------------Methods implementing defaultPolicy------------------

defaultPolicy = undefined







------------------Methods implementing backUp------------------

backUp = undefined




