module Arbitrary.MCTS where

import Test.Tasty.QuickCheck as QC
import Control.Monad
import Control.Applicative

import AI.MCTS

instance Arbitrary Reward where
    arbitrary = elements [Minus, Plus, Draw]




