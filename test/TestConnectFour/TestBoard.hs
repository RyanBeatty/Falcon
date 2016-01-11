module TestConnectFour.TestBoard where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Monad

import ConnectFour.Board
import ConnectFour.Square
import ConnectFour.Move

import Arbitrary.ConnectFour


-- | Entire test suite
boardTests :: TestTree
boardTests = testGroup "Board: Tests" [boardProperties, boardHUnitTests]

-- | all QuickCheck and SmallCheck property tests
boardProperties :: TestTree
boardProperties = testGroup "Board: Properties" [
  

      QC.testProperty "canMove to emptyColumns" $
        propEmptyColumns


--------Need to refactor tests under here-------------------

    , QC.testProperty "fullboard is not playable" $
        \board -> checkPlayable (filledBoard board) == False

    , QC.testProperty "almost filled board is playable" $
        \board -> checkPlayable (almostFilledBoard board) == True

    , QC.testProperty "board that has a vertical win is a won board" $
        \board -> checkWon (columnWonBoard board) == checkWonColumns redSquare (columnWonBoard board) || checkWonColumns blackSquare (columnWonBoard board) == True

    , QC.testProperty "board that has a horizontal win is a won board" $
        \board -> checkWon (rowWonBoard board) == checkWonRows redSquare (rowWonBoard board) || checkWonRows blackSquare (rowWonBoard board) == True

    , QC.testProperty "canMove with FilledBoard == False " $
        \board -> (and . map (flip canMove (filledBoard board))) [(One)..(Seven)] == False

    , QC.testProperty "Test that placeSquareInColumn works for multiple inserts" $
        forAll (choose (1, numRows-1)) $ \n -> ((!! n) . iterate (placeSquareInColumn redSquare)) (replicate numRows emptySquare) == (replicate (numRows-n) emptySquare) ++ (replicate n redSquare)
    ]

-- | Tests that a player can move to all columns returned by emptyColumns
propEmptyColumns board = and . map (flip canMove board) . emptyColumns $ board


-- | all HUnit tests
boardHUnitTests :: TestTree
boardHUnitTests  = testGroup "Board: Unit Tests" 
    [testCheckPlayable
    , testCheckWonDiagonals
    , testCanMove
    --, testPlaceSquareInColumn
    ]

-- | HUnit tests for canMove
testCanMove = testGroup "canMove: HUnit Tests" $
    [ testCase "player can move in initialBoard" $
        (and . map (flip canMove initialBoard)) [(One)..(Seven)] @?= True
    ]

--testPlaceSquareInColumn = testGroup "placeSquareInColumn: HUnit Tests" $
--    [ testCase "Tests that placeSquareInColumn n times filles up Column" $
--        ((!! numRows) . iterate (placeSquareInColumn blackSquare)) (replicate numRows emptySquare) @?= replicate numRows blackSquare
--    ]
--    where 

-- | HUnit tests for checkWonDiagonals
-- | TODO: maybe try to get QuickCheck to auto-generate tests
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




