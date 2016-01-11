module ConnectFour.GameState where

import Control.Applicative

import ConnectFour.Board
import ConnectFour.Piece
import ConnectFour.Move

data GameState = GameState { 
                 activePlayer :: Piece
               , board        :: Board 
               , boardState   :: BoardState
               } deriving (Show, Eq)

gameState :: Piece -> Board -> GameState
gameState piece board = GameState piece board (getBoardState board)

gamePlayable :: GameState -> Bool
gamePlayable gstate = boardPlayable == boardState gstate

gameDrawn :: GameState -> Bool
gameDrawn gstate = boardPlayable == boardState gstate

gameWon :: GameState -> Bool
gameWon gstate = boardWon == boardState gstate

-- | initial state of the game
initialGameState :: GameState
initialGameState = gameState redPiece initialBoard 

-- | Returns a list of valid columns to place a piece in
validColumns :: GameState -> [Column]
validColumns gstate
     | boardState gstate == boardPlayable = emptyColumns (board gstate)
     | otherwise                          = []

-- | Makes a move and updates the state of the game
updateGameState :: GameState -> Move -> Maybe GameState
updateGameState gstate move = gameState nextPlayer <$> board'
    where nextPlayer = oppositePiece (activePlayer gstate) 
          board'     = updateBoard move (board gstate)




