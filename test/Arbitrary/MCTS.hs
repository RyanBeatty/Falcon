{-# LANGUAGE FlexibleInstances #-}
module Arbitrary.MCTS where

import Test.Tasty.QuickCheck as QC
import Control.Monad
import Control.Applicative
import Data.Tree
import Data.Maybe

import ConnectFour.GameState
import ConnectFour.Move
import ConnectFour.Piece

import Arbitrary.ConnectFour
import AI.MCTS

-- | Generates a random Reward
instance Arbitrary Reward where
    arbitrary = elements [Minus, Plus, Draw]


-- | Generates a valid SearchNode. Makes sure
-- | that the visitCount and value fields for
-- | the node are valid
instance Arbitrary SearchNode where
    arbitrary = uncurry3 . uncurry searchNode <$>
                genValueAndVisitCount <*>   -- gen valid value and visitCount
                (arbitrary >>= genRewardAndActionFromGamestate) -- gen valid reward, action, and gamestate
        where genValueAndVisitCount = do
                count <- arbitrary `suchThat` (>0)
                val   <- choose ((-1) * count, count)
                return (val, count)

              uncurry3 f (a,b,c) = uncurry (f a) (b,c)

genRewardAndActionFromGamestate gstate
    | gameDrawn gstate = triple <$> pure Draw  <*> genMoveFromBoard board' `suchThat` ((/=) curPlayer . piece) <*> pure gstate
    | otherwise        = triple <$> genNotDraw <*> genMoveFromBoard board' `suchThat` ((/=) curPlayer . piece) <*> pure gstate
    where genNotDraw   = arbitrary `suchThat` (Draw /=)
          board'       = board gstate 
          curPlayer    = activePlayer gstate
          triple a b c = (a,b,c)


instance Arbitrary (Tree SearchNode) where
  arbitrary = arbitrary `suchThat` ((==) Minus . reward) >>= genSearchTreeFromSearchNode

genSearchTreeFromSearchNode root = do
    let nextReward = flipReward $ reward root
        moves      = validMoves (state root)
        states     = map (updateGameState (state root)) moves
        choices    = map emptyTree . catMaybes $ zipWith fmap (map (newSearchNode nextReward) moves) states
    children <- sublistOf choices
    return $ Node root children 
