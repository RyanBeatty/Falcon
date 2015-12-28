

import ConnectFour

import Data.Maybe
import System.IO

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          gameLoop initialGameState
          
gameLoop :: GameState -> IO ()
gameLoop gstate = do displayBoard (board gstate)
                     let curPlayer = activePlayer gstate
                     putStr $ (pieceString curPlayer)++ " Player, choose a column number to move: "
                     colChoice <- getLine
                     let column = readColumn colChoice

                     let nextState = column >>= (flip updateGameState gstate)
                     case nextState of
                        Nothing         -> do putStrLn "Invalid move"
                                              gameLoop gstate
                        (Just nextSate) -> gameLoop nextSate

                     --if isNothing nextState then do
                     --   putStrLn "Invalid move"
                     --   gameLoop gstate
                     --   else
                     --       gameloop $ maybe gstate 
                     --if isNothing column then do
                     --   putStrLn "Invalid column choice"
                     --   gameLoop gstate
                     --   else do
                     --       let nextState = updateGameState column gstate
                     --       if isNothing nextState then do
                     --           putStrLn "Column is full"
                     --           gameLoop gstate
                     --           else
                     --               gameLoop gstate

displayBoard :: Board -> IO ()
displayBoard = putStr . ("\n" ++) . showBoard

--getMove :: Piece -> IO (Move)
--getMove piece = do putStr $ (showPieceString piece) ++ " Player choose a column number: "
--                   colChoice <- getLine
--                   let column = readColumn colChoice
--                   if isNothing column then
--                       putStrLn "Invalid column choice"
--                       getMove piece
--                       else
--                           return column

