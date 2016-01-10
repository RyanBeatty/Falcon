module ConnectFour.GameState where

import ConnectFour.Board
import ConnectFour.Piece
import ConnectFour.Move

data GameState = GameWon Piece
               | GameDraw 
               | GameState 
               { board :: Board 
               , activePlayer :: Piece
               } deriving (Show, Eq)

gameWon :: Piece -> GameState
gameWon = GameWon

gameDraw :: GameState
gameDraw = GameDraw

gameState :: Board -> Piece -> GameState
gameState = GameState

--gameState :: Board -> Piece -> GameState
--gameState board piece = 
--     case getBoardState board of
--          BoardPlayable -> GameState board (oppositePiece piece)
--          BoardWon      -> gameWon piece
--          BoardDraw     -> gameDraw



-- | initial state of the game
initialGameState :: GameState
initialGameState = gameState initialBoard redPiece 

-- | Returns a list of valid columns to place a piece in
validColumns :: GameState -> [Column]
validColumns (GameState board _) = emptyColumns board
validColumns _                   = []

-- | Makes a move and updates the state of the game
updateGameState :: GameState -> Move -> Maybe GameState
updateGameState state move = 
     board' >>= 
     \brd -> case getBoardState brd of
          BoardPlayable -> Just $ GameState brd nextPlayer
          BoardWon      -> Just $ GameWon curPlayer 
          BoardDraw     -> Just $ GameDraw
    where curPlayer  = activePlayer state
          nextPlayer = oppositePiece curPlayer 
          board'     = updateBoard move (board state)




