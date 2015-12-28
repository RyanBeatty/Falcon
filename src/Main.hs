

import ConnectFour

import Data.Maybe
import System.IO

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          gameLoop initialGameState
          
gameLoop :: GameState -> IO ()
gameLoop gstate = case gstate of
                    (GameState _ _)  -> makeMove gstate >>= gameLoop
                    (GameWon player) -> putStrLn $ (pieceString player) ++ " Player has won!"
                    (GameDraw)       -> putStrLn "Its a draw!"


getMove :: GameState -> IO (Move)
getMove gstate = do displayBoard (board gstate)
                    putStr $ (pieceString curPlayer)++ " Player, choose a column number to move: "
                    columnChoice <- getLine >>= return . readColumn
                    let validColumn = maybe False (flip elem moves) columnChoice
                    if validColumn then
                        (\(Just column) -> return $ move column curPlayer) columnChoice
                        else do putStrLn "Invalid Move. Please choose another move"
                                getMove gstate
    where curPlayer = activePlayer gstate
          moves = validColumns gstate 


makeMove :: GameState -> IO (GameState)
makeMove gstate = do displayBoard (board gstate)
                     let curPlayer = activePlayer gstate
                     putStr $ (pieceString curPlayer)++ " Player, choose a column number to move: "
                     colChoice <- getLine
                     let column = readColumn colChoice

                     let nextState = column >>= (flip updateGameState gstate)
                     case nextState of
                         Nothing         -> do putStrLn "Invalid move"
                                               makeMove gstate
                         (Just nextSate) -> return nextSate

displayBoard :: Board -> IO ()
displayBoard = putStr . ("\n" ++) . showBoard

