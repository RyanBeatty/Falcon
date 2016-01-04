module MCTS where

import ConnectFour.GameState (GameState)

import Data.Tree
import Data.Tree.Zipper
import Data.List

data SearchNode = TerminalNode 
                { state :: GameState
                }
                | SearchNode 
                { value      :: Int
                , visitCount :: Int
                , state      :: GameState
                }

type SearchTree = Tree SearchNode

--mctsSearch :: GameTree -> Game
--mctsSearch root = backUp . defaultPolicy . treePolicy

-- | Search faze of a single iteration of MCTS. Decends down the tree
-- | deciding until it finds either a terminal node or a node that is not
-- | fully expanded.
treePolicy :: TreePos Full SearchNode -> TreePos Full SearchNode
treePolicy searchTree
  | isTerminal tree'      = searchTree
  | isFullyExpanded tree' = treePolicy $ focusNthChild bcIndex searchTree
  | otherwise             = modifyTree expand searchTree
  where tree'   = tree searchTree
        bcIndex = bestChildIndex $ tree searchTree

focusNthChild :: Int -> TreePos Full SearchNode -> TreePos Full SearchNode
focusNthChild n tree = case childAt n tree of
                        (Just child) -> child

--treePolicy :: SearchTree -> SearchTree
--treePolicy gtree
--    | isTerminal      = gtree
--    | isFullyExpanded = treePolicy $ bestChild gtree
--    | otherwise       = expand gtree

expand :: SearchTree -> SearchTree
expand = undefined

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




