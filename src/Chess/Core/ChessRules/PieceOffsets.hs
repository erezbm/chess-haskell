module Chess.Core.ChessRules.PieceOffsets (pieceOffsetsCR) where

import Chess.Core.Models
import Chess.Core.ChessRule (IllegalMoveError (PieceTypeDestError), ChessRule, askMove, errorCR)
import Chess.Core.ChessRules.PieceExists (pieceExistsCR)
import Control.Monad (unless)

pieceOffsetsCR :: ChessRule ()
pieceOffsetsCR = do
  piece <- pieceExistsCR
  Move from to <- askMove
  unless (to `elem` applyPieceOffsets (getPieceOffsets piece) from) (errorCR PieceTypeDestError)
