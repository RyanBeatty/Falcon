module TestConnectFour.TestGameState where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Maybe

import Arbitrary.ConnectFour
import ConnectFour.GameState
import ConnectFour.Board
import ConnectFour.Move

-- | Entire test suite
gameStateTests :: TestTree
gameStateTests = testGroup "GameState: Tests" [
          QC.testProperty "canMove for all validColumns" $
            propCanMoveValidColumns

        --, QC.testProperty "updateGameState board == updateBoard" $
        --    propUpdateGameState
    ]

propCanMoveValidColumns gstate = and . map (flip canMove board') . validColumns $ gstate
    where board' = board gstate

propUpdateGameState gstate move' = 
    move' `elem` map (flip move (activePlayer gstate)) (validColumns gstate) &&
    isPlayable (fromJust updatedState) ==>
        updatedState /= Nothing &&
        fmap board updatedState == updateBoard move' (board gstate)
    where updatedState = updateGameState gstate move'
          isPlayable gs = case gs of
                            (GameState _ _) -> True
                            _               -> False


-- | all QuickCheck and SmallCheck property tests
gameStateProperties :: TestTree
gameStateProperties = testGroup "GameState: Properties" []


-- | all HUnit tests
gameStateHUnitTests :: TestTree
gameStateHUnitTests  = testGroup "GameState: Unit Tests" []




