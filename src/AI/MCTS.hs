module AI.MCTS where

import ConnectFour.GameState (GameState(..), validColumns, activePlayer, updateGameState)
import ConnectFour.Move (Move(..), Column(..), move, columns)
import ConnectFour.Piece (Piece(..))

import Data.Tree
import Data.Tree.Zipper as Zipper
import Data.List
import System.Random

type Action = Move

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

--instance Eq SearchNode where
--  (TerminalNode r1) == (TerminalNode r2) = r1 == r2
--  (TerminalNode _) == _                  = False
--  _ == (TerminalNode _)                  = False
--  (SearchNode a1 b1 c1 d1 e1) == (SearchNode a2 b2 c2 d2 e2) = (a1==a2) && 
--                                                               (b1==b2) && 
--                                                               (c1==c2) && 
--                                                               (d1==d2) && 
--                                                               (e1==e2)

--instance Ord SearchNode where
--  compare (TerminalNode r1) (TerminalNode r2) = compare r1 r2
--  compare (TerminalNode Plus) _               = GT
--  compare _ (TerminalNode Plus)               = LT
--  compare (TerminalNode _) _                  = LT
--  compare _                (TerminalNode _)   = LT
--  compare (SearchNode v1 _ _ _ _) (SearchNode v2 _ _ _ _) = compare v1 v2

emptyTree :: SearchNode -> SearchTree
emptyTree node = Node node []

searchNode :: Int -> Int -> Reward -> Action -> GameState -> SearchNode
searchNode value count reward action curState = 
  case curState of
    (GameState _ _) -> SearchNode value count reward action curState False
    (GameDraw)      -> SearchNode value count Draw   action curState True
    _               -> SearchNode value count reward action curState True   

newSearchNode :: Reward -> Action -> GameState -> SearchNode
newSearchNode = searchNode 0 0

rootSearchNode :: GameState -> SearchNode
rootSearchNode = newSearchNode Minus (Move One RedPiece) 

mctsSearch :: TreePos Full SearchNode -> StdGen -> (TreePos Full SearchNode, StdGen)
mctsSearch tree = uncurry3 backUp . uncurry defaultPolicy . treePolicy tree
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
--isTerminal tree = case rootLabel tree of
--                    (TerminalNode _) -> True
--                    _                -> False

-- | A node is fully expanded if it has the same amount of children
-- | as there are Column choices.
isFullyExpanded :: SearchTree -> Bool
isFullyExpanded tree = (length . subForest $ tree) == length columns

-- | Returns the best child node of a root node.
-- | The best child is the child with the highest win value
--bestChild :: SearchTree -> SearchNode
--bestChild = undefined
--bestChild = maximum . map rootLabel . subForest

-- | Returns the index of the best child node
--bestChildIndex :: SearchTree -> Int
--bestChildIndex = undefined
--bestChildIndex tree = case elemIndex bChild children of
--                        Nothing      -> error "should not happen"
--                        (Just index) -> index
--  where bChild   = bestChild tree
--        children = map rootLabel . subForest $ tree

cp :: Double
cp = 1 / (sqrt $ fromIntegral 2)

childValue :: Double -> Int -> SearchNode -> Double
childValue weight nParent node = (q/n) + weight * (sqrt $ (2 * log (fromIntegral nParent)) / n)
  where q = fromIntegral (value node) :: Double
        n = fromIntegral (visitCount node) :: Double

bestChildIndex :: SearchTree -> Int
bestChildIndex tree = case elemIndex (maximum children) children of
                        (Just index) -> index
                        Nothing      -> error "no maximum child"
  where rootVal  = value . rootLabel $ tree
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
          newState            = applyAction newAction curState
          newNode             = Node (newSearchNode newReward newAction newState) []

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
        newState            = applyAction newAction (state node)
        newReward           = flipReward . reward $ node
        newNode             = newSearchNode newReward newAction newState


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



