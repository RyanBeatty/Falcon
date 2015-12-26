module TestBoard where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Monad

import ConnectFour.Board
import ConnectFour.Square

import Utils


-- | Entire test suite
boardTests :: TestTree
boardTests = testGroup "Board: Tests" [boardProperties, boardHUnitTests]

-- | all QuickCheck and SmallCheck property tests
boardProperties :: TestTree
boardProperties = testGroup "Board: Properties" 
    [ QC.testProperty "fullboard is not playable" $
        \board -> checkPlayable (filledBoard board) == False

    , QC.testProperty "almost filled board is playable" $
        \board -> checkPlayable (almostFilledBoard board) == True

    , QC.testProperty "board that has a vertical win is a won board" $
        \board -> checkWon (columnWonBoard board) == checkWonColumns redSquare (columnWonBoard board) || checkWonColumns blackSquare (columnWonBoard board) == True

    , QC.testProperty "board that has a horizontal win is a won board" $
        \board -> checkWon (rowWonBoard board) == checkWonRows redSquare (rowWonBoard board) || checkWonRows blackSquare (rowWonBoard board) == True
    ]


-- | all HUnit tests
boardHUnitTests :: TestTree
boardHUnitTests  = testGroup "Board: Unit Tests" [testCheckPlayable, testCheckWonDiagonals]


testCheckWonDiagonals = testGroup "checkWonDiagonals: HUnit Tests" $
    [ testCase "Check that an empty board is not won on the diagonals" $
        checkWonDiagonals redSquare initialBoard @?= False

    , testCase "check that the board is won if there is four-in-a-row on a main diagonal" $
        checkWonDiagonals redSquare mainDiagonalBoard @?= True

    , testCase "Check that the board is won if ther is four-in-a-row on an antidiagonal" $
        checkWonDiagonals blackSquare antiDiagonalBoard @?= True
    ]
    where mainDiagonalBoard = [ replicate numRows emptySquare
                              , replicate numRows emptySquare
                              , [emptySquare, emptySquare, emptySquare, redSquare, emptySquare, emptySquare]
                              , [emptySquare, emptySquare, redSquare, emptySquare, emptySquare, emptySquare]
                              , [emptySquare, redSquare, emptySquare, emptySquare, emptySquare, emptySquare]
                              , [redSquare, emptySquare, emptySquare, emptySquare, emptySquare, emptySquare]
                              , replicate numRows emptySquare
                              ]
          antiDiagonalBoard = [ replicate numRows emptySquare
                              , replicate numRows emptySquare
                              , [blackSquare, emptySquare, emptySquare, emptySquare, emptySquare, emptySquare]
                              , [emptySquare, blackSquare, emptySquare, emptySquare, emptySquare, emptySquare]
                              , [emptySquare, emptySquare, blackSquare, emptySquare, emptySquare, emptySquare]
                              , [emptySquare, emptySquare, emptySquare, blackSquare, emptySquare, emptySquare]
                              , replicate numRows emptySquare
                              ]

-- | Tests that boards that are not full are playable.
-- | Boards that are full are not playable
testCheckPlayable = testGroup "checkPlayable: HUnit Tests" 
    [ testCase "check that board is playable in initial state" $
        checkPlayable initialBoard @?= True

    , testCase "check that board is not playable when full" $
        checkPlayable fullBoard @?= False

    , testCase "Check that a board is playable when almost full" $
        checkPlayable almostFullBoard @?= True
    ]
    where fullBoard = replicate numCols (replicate numRows redSquare)
          almostFullBoard = [[emptySquare] ++ replicate (numRows-1) redSquare] ++ replicate (numCols-1) (replicate numRows redSquare)




