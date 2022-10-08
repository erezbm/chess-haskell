module Chess.Core.ChessRules.PieceExists where

import Chess.Core.ChessRule
import Chess.Core.Models

pieceExistsCR :: ChessRule Piece
pieceExistsCR = do
  gameState <- askGameState
  move <- askMove
  maybe (errorCR EmptySquareError) return (getPiece (board gameState) (moveSource move))
