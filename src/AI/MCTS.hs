module AI.MCTS where

import ConnectFour.GameState (GameState(..), validColumns, activePlayer, updateGameState)
import ConnectFour.Move (Move, Column, move, columns)

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
                } deriving (Show, Eq)

type SearchTree = Tree SearchNode

gameNode :: Int -> Int -> Action -> GameState -> SearchNode
gameNode value count action curState = case curState of
                                        (GameState _ _) -> SearchNode value count action curState
                                        _             -> TerminalNode curState  

newGameNode :: Action -> GameState -> SearchNode
newGameNode action curState = gameNode 0 0 action curState

mctsSearch :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
mctsSearch = (,) . backUp . defaultPolicy . treePolicy

------------------methods implementing treePolicy------------------

-- | Search faze of a single iteration of MCTS. Decends down the tree
-- | deciding until it finds either a terminal node or a node that is not
-- | fully expanded.
treePolicy :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
treePolicy searchTree gen
  -- If a node is a TerminalNode, then stop search
  | isTerminal tree'      = (searchTree, gen)

  -- If a Node is FullyExpanded, then continue search with its best child
  | isFullyExpanded tree' = treePolicy bChild gen

  -- otherwise, expand the current node and stop search
  | otherwise             = expand searchTree gen

  where tree'            = tree searchTree
        
        -- Gets the index for the best child of the current node
        bcIndex          = bestChildIndex $ tree searchTree
        
        -- Gets the best child of the current node
        bChild           = case childAt bcIndex searchTree of
                               (Just child) -> child   

isTerminal :: SearchTree -> Bool
isTerminal tree = case rootLabel tree of
                    (TerminalNode _) -> True
                    _                -> False

-- | A node is fully expanded if it has the same amount of children
-- | as there are Column choices.
isFullyExpanded :: SearchTree -> Bool
isFullyExpanded tree = (length . subForest $ tree) == length columns

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
expand searchTree gen = (modifyTree (addChild newNode) searchTree, newGen)
    where tree'            = tree searchTree
          curState         = state . rootLabel $ tree'
          (action, newGen) = chooseAction tree' gen
          newState         = applyAction action curState
          newNode          = newGameNode action newState

-- | Chooses a new action to take using the rng and based off
-- | of which actions have already been chosen 
chooseAction :: SearchTree -> StdGen -> (Action, StdGen)
chooseAction searchTree gen = (actions !! choice, newGen)
    where chosen            = getChildrenActions searchTree
          possible          = possibleActions $ rootLabel searchTree
          actions           = filter (`notElem` chosen) possible
          (choice, newGen)  = randomR (0, length actions - 1) gen 

-- | Adds a new child node to a tree as a child of the root node
addChild :: SearchNode -> SearchTree -> SearchTree
addChild child tree = tree {subForest = child' : children}
    where child' = Node child []
          children = subForest tree


-- | Returns a list of all the chosen actions from a root node 
getChildrenActions :: SearchTree -> [Action]
getChildrenActions = map (action . rootLabel) . subForest

-- | Returns a list of all of the possible actions that can be chosen
-- | from a certain SearchNode
possibleActions :: SearchNode -> [Action]
possibleActions searchNode = map col2Move $ validColumns gameState
    where gameState = state searchNode
          curPlayer = activePlayer gameState
          col2Move  = flip move curPlayer

-- | Updates the current game state with the chosen action
-- | NOTE: Only use with a valid action
applyAction :: Action -> GameState -> GameState
applyAction action oldState = case updateGameState oldState action of
                                Nothing         -> error "this should not happen"
                                (Just newState) -> newState 

------------------Methods implementing defaultPolicy------------------

defaultPolicy = undefined







------------------Methods implementing backUp------------------

backUp = undefined




