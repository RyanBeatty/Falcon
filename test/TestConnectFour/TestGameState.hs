module TestConnectFour.TestGameState where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Arbitrary.ConnectFour
import ConnectFour.GameState
import ConnectFour.Board
import ConnectFour.Move

-- | Entire test suite
gameStateTests :: TestTree
gameStateTests = testGroup "GameState: Tests" [
          QC.testProperty "canMove for all validColumns" $
            propCanMoveValidColumns

        , QC.testProperty "board . updateGameState == updateBoard" $
            propUpdateGameState
    ]

-- | tests that the columns that are returned by validColumns
-- | are not full columns
propCanMoveValidColumns gstate = and . map (flip canMove board') . validColumns $ gstate
    where board' = board gstate

-- | Tests that updating a board with a move is
-- | equal to the board of an updated gamestate with the same move
propUpdateGameState gstate move' = 
    move' `elem` map (flip move (activePlayer gstate)) (validColumns gstate) ==>
        updatedState /= Nothing &&
        fmap board updatedState == updateBoard move' (board gstate)
    where updatedState = updateGameState gstate move'


-- | all QuickCheck and SmallCheck property tests
gameStateProperties :: TestTree
gameStateProperties = testGroup "GameState: Properties" []


-- | all HUnit tests
gameStateHUnitTests :: TestTree
gameStateHUnitTests  = testGroup "GameState: Unit Tests" []




