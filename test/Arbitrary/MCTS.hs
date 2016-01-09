module Arbitrary.MCTS where

import Test.Tasty.QuickCheck as QC
import Control.Monad
import Control.Applicative

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
                genRewardAndActionAndGameState -- gen valid reward, action, and gamestate
        where genValueAndVisitCount = do
                count <- arbitrary `suchThat` (>0)
                val   <- choose ((-1) * count, count)
                return (count, val)

              -- | Returns random, valid reward, action, and gamestate
              genRewardAndActionAndGameState = do
                gstate <- arbitrary
                case gstate of
                    (GameState board p) -> triple <$> genNotDraw <*> genMoveFromBoard board `suchThat` ((/=) p . piece) <*> pure gstate
                    (GameWon p)         -> triple <$> genNotDraw <*> arbitrary `suchThat` ((==) p . piece) <*> pure gstate
                    (GameDraw)          -> triple <$> pure Draw  <*> arbitrary <*> pure gstate
                where genNotDraw = arbitrary `suchThat` (Draw /=)
                      genMove genM p = genM `suchThat` ((/=) p . piece)
                      triple a b c = (a,b,c)

              uncurry3 f (a,b,c) = uncurry (f a) (b,c)


