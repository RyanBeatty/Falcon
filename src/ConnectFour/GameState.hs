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

-- | Constructs a new GameState from the passed in piece and board.
gameState :: Piece -> Board -> GameState
gameState piece board = GameState piece board (getBoardState board)

-- | Returns true if the board is still playable
gamePlayable :: GameState -> Bool
gamePlayable gstate = boardPlayable == boardState gstate

-- | Returns true if the game is a draw
gameDrawn :: GameState -> Bool
gameDrawn gstate = boardDrawn == boardState gstate

-- | Returns true if a player has won
gameWon :: GameState -> Bool
gameWon gstate = boardWon == boardState gstate

-- | initial state of the game
initialGameState :: GameState
initialGameState = gameState redPiece initialBoard 

-- | Returns a list of the valid moves from the current state.
-- | If the board is not playable, then there are no valid moves
validMoves :: GameState -> [Move]
validMoves gstate
     | boardState gstate == boardPlayable = map (flip move curPlayer) . emptyColumns $ board gstate
     | otherwise                          = []
     where curPlayer = activePlayer gstate

-- | Makes a move and updates the state of the game
updateGameState :: GameState -> Move -> Maybe GameState
updateGameState gstate move
     | not (gamePlayable gstate) = Nothing
     | otherwise                 = gameState nextPlayer <$> board'
    where nextPlayer = oppositePiece (activePlayer gstate) 
          board'     = updateBoard move (board gstate)




