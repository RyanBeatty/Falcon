module MCTS where

import ConnectFour.GameState (GameState, validColumns, activePlayer)
import ConnectFour.Move (Move, Column, move)

import Data.Tree
import Data.Tree.Zipper
import Data.List
import System.Random

type Action = Move

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

-- | Takes the current zipper position in the search tree
-- | uses the rng to choose a new action to take and adds
-- | the new node to the tree
expand :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
expand searchTree gen = undefined
    where tree'            = tree searchTree
          (action, newGen) = chooseAction tree' gen
          newNode          = newGameNode action (state . rootLabel $ tree') 

-- | Chooses a new action to take using the rng and based off
-- | of which actions have already been chosen 
chooseAction :: SearchTree -> StdGen -> (Action, StdGen)
chooseAction searchTree gen = (actions !! choice, newGen)
    where chosen            = getChildrenActions searchTree
          possible          = possibleActions searchTree
          actions           = [a | a<-possible, a `notElem` chosen]
          (choice, newGen)  = randomR (0, length actions - 1) gen 

-- | Returns a list of all the chosen actions from a root node 
getChildrenActions :: SearchTree -> [Action]
getChildrenActions = map (action . rootLabel) . subForest

-- | Returns a list of all of the possible actions that can be chosen
possibleActions :: SearchTree -> [Action]
possibleActions searchTree = map col2Move $ validColumns gameState
    where gameState = state . rootLabel $ searchTree
          curPlayer = activePlayer gameState
          col2Move  = flip move curPlayer 

------------------Methods implementing defaultPolicy------------------

defaultPolicy = undefined







------------------Methods implementing backUp------------------

backUp = undefined




