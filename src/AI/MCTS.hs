module AI.MCTS where

import ConnectFour.GameState
import ConnectFour.Move (Move(..), Column(..), columns)
import ConnectFour.Piece (Piece(..))

import Data.Tree
import Data.Tree.Zipper as Zipper
import Data.List
import Data.Maybe
import System.Random

type Action = Move
type Budget = Int

data Reward = Minus | Draw | Plus
  deriving (Show, Eq, Ord)

data SearchNode = SearchNode { 
                  value      :: Int
                , visitCount :: Int
                , reward     :: Reward
                , action     :: Action
                , state      :: GameState
                , terminal   :: Bool
                } deriving (Show)

type SearchTree = Tree SearchNode

------------------Constructor methods------------------

searchNode :: Int -> Int -> Reward -> Action -> GameState -> SearchNode
searchNode value count reward action gstate 
  | gamePlayable gstate = SearchNode value count reward action gstate False
  | gameDrawn gstate    = SearchNode value count Draw   action gstate True
  | gameWon gstate      = SearchNode value count reward action gstate True

newSearchNode :: Reward -> Action -> GameState -> SearchNode
newSearchNode = searchNode 0 0

rootSearchNode :: GameState -> SearchNode
rootSearchNode = newSearchNode Minus (Move One RedPiece) 


emptyTree :: SearchNode -> SearchTree
emptyTree node = Node node []

------------------Methods implementing Monte-Carlo Tree Search Algorithm------------------


mcts :: Budget -> GameState -> StdGen -> Action
mcts budget gstate gen = action . bestChild . (\(stree, _) -> tree stree) $ search budget (fromTree . emptyTree $ rootSearchNode gstate) gen


search :: Budget -> TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
search budget tree gen
  | budget > 0 = uncurry (search (budget-1)) . uncurry3 backUp . uncurry defaultPolicy $ treePolicy tree gen
  | otherwise  = (tree, gen)
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
                               Nothing      -> error "no best child"   

isTerminal :: SearchTree -> Bool
isTerminal = terminal . rootLabel

-- | A node is fully expanded if it has the same amount of children
-- | as there are Column choices.
isFullyExpanded :: SearchTree -> Bool
isFullyExpanded tree = (length . subForest $ tree) == (length . possibleActions $ tree)

cp :: Double
cp = 1 / (sqrt $ fromIntegral 2)

childValue :: Double -> Double -> SearchNode -> Double
childValue weight valParent node = (q/n) + weight * (sqrt $ (2 * log valParent) / n)
  where q = fromIntegral (value node) :: Double
        n = fromIntegral (visitCount node) :: Double

bestChild :: SearchTree -> SearchNode
bestChild tree = (map rootLabel . subForest $ tree) !! bestChildIndex tree

bestChildIndex :: SearchTree -> Int
bestChildIndex tree = case elemIndex (maximum children) children of
                        (Just index) -> index
                        Nothing      -> error "no maximum child"
  where rootVal  = fromIntegral . value . rootLabel $ tree
        children = map (childValue cp rootVal) . map rootLabel . subForest $ tree

------------------Methods implementing expand------------------

-- | Takes the current zipper position in the search tree
-- | uses the rng to choose a new action to take and adds
-- | the new node to the tree
expand :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
expand searchTree gen = (Zipper.insert newNode . children $ searchTree, newGen)
    where tree'               = tree searchTree
          curState            = state . rootLabel $ tree'
          newReward           = flipReward . reward . rootLabel $ tree'
          (newAction, newGen) = chooseAction tree' gen
          newState            = applyAction curState newAction
          newNode             = Node (newSearchNode newReward newAction newState) [] 

-- | Chooses a new action to take using the rng and based off
-- | of which actions have already been chosen 
chooseAction :: SearchTree -> StdGen -> (Action, StdGen)
chooseAction searchTree gen = (actions !! choice, newGen)
    where chosen            = childrenActions searchTree
          possible          = possibleActions searchTree
          actions           = filter (`notElem` chosen) possible
          (choice, newGen)  = randomR (0, length actions - 1) gen 

-- | Returns a list of all the chosen actions from a root node 
childrenActions :: SearchTree -> [Action]
childrenActions = map (action . rootLabel) . subForest

-- | Returns a list of all of the possible actions that can be chosen
-- | from a certain SearchNode
possibleActions :: SearchTree -> [Action]
possibleActions = validMoves . state . rootLabel

-- | Updates the current game state with the chosen action
-- | NOTE: Only use with a valid action
applyAction :: GameState -> Action -> GameState
applyAction oldState = fromJust . updateGameState oldState

flipReward :: Reward -> Reward
flipReward Plus  = Minus
flipReward Minus = Plus 

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
  where (newAction, newGen) = chooseAction (emptyTree node) gen
        newState            = applyAction (state node) newAction
        newReward           = flipReward . reward $ node
        newNode             = newSearchNode newReward newAction newState


------------------Methods implementing backUp------------------

backUp :: Reward -> TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
backUp reward searchTree gen = 
  case parent updatedTree of
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
                        _     -> value node



