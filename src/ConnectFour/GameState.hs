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
                                BoardPlayable -> GameState board' nextPiece
                                BoardWon      -> GameWon curPiece 
                                BoardDraw     -> GameDraw
    where curPiece  = piece state
          nextPiece = oppositePiece curPiece 
          board'    = updateBoard move curPiece (board state)




