module MCTS where

import ConnectFour.GameState (GameState)

import Data.Tree
import Data.Tree.Zipper
import Data.List
import System.Random

data SearchNode = TerminalNode 
                { state :: GameState
                }
                | SearchNode 
                { value      :: Int
                , visitCount :: Int
                , state      :: GameState
                }

type SearchTree = Tree SearchNode

--gameNode :: Int -> Int -> Move -> SearchNode
--gameNode = undefined 

mctsSearch :: StdGen -> TreePos Full SearchNode -> (StdGen, TreePos Full SearchNode)
mctsSearch = (,) . backUp . defaultPolicy . treePolicy

------------------methods implementing treePolicy------------------

-- | Search faze of a single iteration of MCTS. Decends down the tree
-- | deciding until it finds either a terminal node or a node that is not
-- | fully expanded.
treePolicy :: StdGen -> TreePos Full SearchNode -> (StdGen, TreePos Full SearchNode)
treePolicy gen searchTree
  -- If a node is a TerminalNode, then stop search
  | isTerminal tree'      = (gen, searchTree)

  -- If a Node is FullyExpanded, then continue search with its best child
  | isFullyExpanded tree' = treePolicy gen bChild

  -- otherwise, expand the current node and stop search
  | otherwise             = (newGen, modifyTree (expand newNode) searchTree)
  
  where tree'             = tree searchTree
        bcIndex           = bestChildIndex $ tree searchTree
        bChild            = case childAt bcIndex searchTree of
                                (Just child) -> child
        (newNode, newGen) = chooseAction gen tree'   

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


expand :: SearchNode -> SearchTree -> SearchTree
expand newNode searchTree = undefined

chooseAction :: StdGen -> SearchTree -> (SearchNode, StdGen)
chooseAction gen searchTree = undefined

--genChoice :: StdGen -> (Int, StdGen)  


------------------Methods implementing defaultPolicy------------------

defaultPolicy = undefined







------------------Methods implementing backUp------------------

backUp = undefined




