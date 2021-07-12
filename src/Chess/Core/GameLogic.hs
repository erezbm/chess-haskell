module Chess.Core.GameLogic where

import Chess.Core.Models
import Chess.Core.MoveConstraints.ValidMoves
import Control.Monad
import Data.Function
import Data.Maybe
import Utils

applyMove :: Move -> GameState -> Maybe GameState
applyMove move@(Move source dest) gameState = do
  let possibleDests = possibleDestSquares gameState source
  boolToMaybe (dest `elem` possibleDests) $ applyMove' move gameState

possibleDestSquares :: GameState -> Square -> [Square]
possibleDestSquares gameState source = do
  piece <- getPiece source (board gameState) & maybeToList
  guard $ piecePlayer piece == turn gameState
  allSquares
    & validMovesMoveConstraint source piece
    & undefined

applyMove' :: Move -> GameState -> GameState
applyMove' = undefined
