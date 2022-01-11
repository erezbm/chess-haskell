module Chess.Core.ChessRules.PieceExists where

import Chess.Core.Models
import Chess.Core.ChessRule (IllegalMoveError (EmptySquareError), ChessRule, askGameState, askMove, errorCR)

pieceExistsCR :: ChessRule Piece
pieceExistsCR = do
  gameState <- askGameState
  move <- askMove
  maybe (errorCR EmptySquareError) return (getPiece (moveSource move) (board gameState))
