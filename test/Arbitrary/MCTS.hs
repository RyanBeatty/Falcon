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
                return (count, val)

              uncurry3 f (a,b,c) = uncurry (f a) (b,c)

genRewardAndActionFromGamestate gstate
    | gameDrawn gstate = triple <$> pure Draw  <*> genMoveFromBoard board' `suchThat` ((/=) curPlayer . piece) <*> pure gstate
    | otherwise        = triple <$> genNotDraw <*> genMoveFromBoard board' `suchThat` ((/=) curPlayer . piece) <*> pure gstate
    where genNotDraw   = arbitrary `suchThat` (Draw /=)
          board'       = board gstate 
          curPlayer    = activePlayer gstate
          triple a b c = (a,b,c)


--genSearchTree = do
--    root  <- arbitrary `suchThat` ((==) Minus . reward)
--    let player   = activePlayer . state $ root
--        moves    = validMoves (state root)
--        states   = map (updateGameState (state root)) moves
--        choices  = zip moves states
--    children <- sublistOf choices
--    uncurry 


    --moves <- sublistOf (possibleActions root)
    --let children = catMaybe $ map (flip applyAction (state root)) moves
    --return $ Node root [children]
