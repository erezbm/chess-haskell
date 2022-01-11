module Chess.Core.ChessRules.Turn (turnCR) where

import Chess.Core.Models
import Chess.Core.ChessRule (IllegalMoveError (OpponentPieceError), ChessRule, askGameState, errorCR)
import Chess.Core.ChessRules.PieceExists (pieceExistsCR)
import Control.Monad (unless)

turnCR :: ChessRule ()
turnCR = do
  piece <- pieceExistsCR
  gameState <- askGameState
  unless (turn gameState == piecePlayer piece) (errorCR OpponentPieceError)
