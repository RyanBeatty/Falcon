module ConnectFour.GameState where

import ConnectFour.Board
import ConnectFour.Piece

data GameState = GameWon Piece
               | GameDraw 
               | GameState 
               { board :: Board 
               , piece :: Piece
               }

gameState :: GameState
gameState = GameState

initialGameState :: GameState
initialGameState = gameState initialBoard redPiece 


updateGameState :: Move -> GameState -> GameState
updateGameState move state = case getBoardState board' of
                                Playable -> GameState board' nextPiece
                                Won      -> GameWon (piece state) 
                                Draw     -> GameDraw
    where board'   = updateBoard move (board state)
          nextPiece = oppositePiece (piece state)




