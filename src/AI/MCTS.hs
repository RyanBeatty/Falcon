module AI.MCTS where

import ConnectFour.GameState (GameState(..), validColumns, activePlayer, updateGameState)
import ConnectFour.Move (Move, Column, move, columns)

import Data.Tree
import Data.Tree.Zipper as Zipper
import Data.List
import System.Random

type Action = Move

data Reward = Plus | Minus | Draw

data SearchNode = TerminalNode 
                { state  :: GameState
                }
                | SearchNode 
                { value      :: Int
                , visitCount :: Int
                , action     :: Action
                , state      :: GameState
                } deriving (Show, Eq)

type SearchTree = Tree SearchNode

emptyTree :: SearchNode -> SearchTree
emptyTree node = Node node []

gameNode :: Int -> Int -> Action -> GameState -> SearchNode
gameNode value count action curState = case curState of
                                        (GameState _ _) -> SearchNode value count action curState
                                        _             -> TerminalNode curState  

newGameNode :: Action -> GameState -> SearchNode
newGameNode action curState = gameNode 0 0 action curState

--mctsSearch :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
mctsSearch tree = (,) . uncurry3 backUp . uncurry defaultPolicy . treePolicy tree
  where uncurry3 f (a,b,c) = uncurry (f a) (b,c) 

------------------methods implementing treePolicy------------------

-- | Search faze of a single iteration of MCTS. Decends down the tree
-- | deciding until it finds either a terminal node or a node that is not
-- | fully expanded.
treePolicy :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
treePolicy searchTree gen
  -- If a node is a TerminalNode, then stop search
  | isTerminal tree'      = (searchTree, gen)

  -- If a Node is FullyExpanded, then search its best child
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
expand searchTree gen = (Zipper.insert newNode . children $ searchTree, newGen)
    where tree'            = tree searchTree
          curState         = state . rootLabel $ tree'
          (action, newGen) = chooseAction tree' gen
          newState         = applyAction action curState
          newNode          = Node (newGameNode action newState) []

-- | Chooses a new action to take using the rng and based off
-- | of which actions have already been chosen 
chooseAction :: SearchTree -> StdGen -> (Action, StdGen)
chooseAction searchTree gen = (actions !! choice, newGen)
    where chosen            = getChildrenActions searchTree
          possible          = possibleActions $ rootLabel searchTree
          actions           = filter (`notElem` chosen) possible
          (choice, newGen)  = randomR (0, length actions - 1) gen 

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

-- | Simulates the completion of a game from the current position
-- | and returns the reward for the current position
defaultPolicy :: TreePos Full SearchNode -> StdGen -> (Reward, TreePos Full SearchNode, StdGen)
defaultPolicy searchTree gen = (reward, searchTree, newGen)
    where rootNode         = rootLabel $ tree searchTree
          (reward, newGen) = simulate rootNode gen

-- | Plays a game to completion, returning the completion reward
simulate :: SearchNode -> StdGen -> (Reward, StdGen)
simulate node gen
  | isTerminal . emptyTree $ node = (reward node, gen)
  | otherwise                     = simulate newNode newGen
  where (action, newGen) = chooseAction (emptyTree node) gen
        newState         = applyAction action (state node)
        newNode          = newGameNode action newState


reward = undefined





------------------Methods implementing backUp------------------

backUp :: Reward -> TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
backUp reward searchTree gen = case parent updatedTree of
                                  Nothing           -> (updatedTree, gen)
                                  (Just parentTree) -> backUp reward parentTree gen
    where updatedTree = modifyTree (updateTree reward) searchTree



updateTree :: Reward -> SearchTree -> SearchTree
updateTree reward tree = tree {rootLabel= updateNode reward (rootLabel tree)}

updateNode :: Reward -> SearchNode -> SearchNode
updateNode reward node = node {value = newValue, visitCount = newCount}
    where newCount = visitCount node + 1
          newValue = case reward of
                        Plus  -> value node + 1
                        Minus -> value node - 1
                        Draw  -> value node 



