module Chess.Core.ChessRules.BasicMove where

import Chess.Core.Models
import Chess.Core.ChessRule (IllegalMoveError (PieceTypeDestError), ChessRule, askMove, errorCR)
import Chess.Core.ChessRules.PieceExists (pieceExistsCR)
import Control.Monad (unless)

basicMoveCR :: ChessRule ()
basicMoveCR = do
  piece <- pieceExistsCR
  Move from to <- askMove
  unless (to `elem` applyPieceOffsets (getPieceBasicOffsets piece) from) (errorCR PieceTypeDestError)
