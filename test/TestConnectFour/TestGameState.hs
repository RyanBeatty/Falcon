module TestConnectFour.TestGameState where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Data.Maybe

import Arbitrary.ConnectFour
import ConnectFour.GameState
import ConnectFour.Board

-- | Entire test suite
gameStateTests :: TestTree
gameStateTests = testGroup "GameState: Tests" [
          QC.testProperty "updateGameState validMoves /= Nothing" $
            propvalidMoves

        , QC.testProperty "board . updateGameState == updateBoard" $
            propUpdateGameState

        , QC.testProperty "updateGameState terminalState == Nothing" $
            propTerminalUpdate
    ]

-- | tests that the player can make all of the moves returned by validMoves
propvalidMoves gstate = all (isJust) . map (updateGameState gstate) . validMoves $ gstate
    where board' = board gstate

-- | Tests that updating a board with a move is
-- | equal to the board of an updated gamestate with the same move
propUpdateGameState gstate move' = 
    move' `elem` validMoves gstate ==>
        updatedState /= Nothing &&
        fmap board updatedState == updateBoard move' (board gstate)
    where updatedState = updateGameState gstate move'

-- | Test that you cannot continue to make moves if the game is over
propTerminalUpdate gstate move =
    gameWon gstate || gameDrawn gstate ==>
        updateGameState gstate move == Nothing


-- | all QuickCheck and SmallCheck property tests
gameStateProperties :: TestTree
gameStateProperties = testGroup "GameState: Properties" []


-- | all HUnit tests
gameStateHUnitTests :: TestTree
gameStateHUnitTests  = testGroup "GameState: Unit Tests" []




