module Arbitrary.MCTS where

import Test.Tasty.QuickCheck as QC
import Control.Monad
import Control.Applicative

import Arbitrary.ConnectFour
import AI.MCTS

instance Arbitrary Reward where
    arbitrary = elements [Minus, Plus, Draw]


instance Arbitrary SearchNode where
    arbitrary = uncurry searchNode <$>
                genValueAndVisitCount <*>
                arbitrary <*>               -- gen reward
                arbitrary <*>               -- gen action
                arbitrary                   -- gen gamestate
        where genValueAndVisitCount = do
                count <- arbitrary `suchThat` (>0)
                val   <- choose ((-1) * count, count)
                return (count, val)


