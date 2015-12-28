module ConnectFour.GameState where

import ConnectFour.Board
import ConnectFour.Piece
import ConnectFour.Move

data GameState = GameWon Piece
               | GameDraw 
               | GameState 
               { board :: Board 
               , activePlayer :: Piece
               }

gameState :: Board -> Piece -> GameState
gameState = GameState

initialGameState :: GameState
initialGameState = gameState initialBoard redPiece 


updateGameState :: Column -> GameState -> Maybe GameState
updateGameState column state = board' >>= \brd -> case getBoardState brd of
                                                  BoardPlayable -> Just $ GameState brd nextPlayer
                                                  BoardWon      -> Just $ GameWon curPlayer 
                                                  BoardDraw     -> Just $ GameDraw
    where curPlayer  = activePlayer state
          nextPlayer = oppositePiece curPlayer 
          move'      = move column curPlayer
          board'     = updateBoard move' (board state)




