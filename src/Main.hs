

import ConnectFour

import Data.Maybe
import System.IO

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          gameLoop initialGameState
          
gameLoop :: GameState -> IO ()
gameLoop gstate = case gstate of
                    (GameState _ _)  -> getMove gstate >>= return . updateGameState gstate >>= maybe (gameLoop gstate) (gameLoop)
                    (GameWon player) -> putStrLn $ (pieceString player) ++ " Player has won!"
                    (GameDraw)       -> putStrLn "Its a draw!"

-- | Displays the board and continues to prompt the user to
-- | enter a move until they enter in a valid move
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

-- | Prints the board to the screen
displayBoard :: Board -> IO ()
displayBoard = putStr . ("\n" ++) . showBoard

