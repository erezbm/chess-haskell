module Chess.Core.MoveConstraints.PieceOffsets (pieceOffsetsMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint (IllegalMoveError (PieceTypeDestError), MoveConstraint, errorMC)
import Chess.Core.MoveConstraints.PieceExists (pieceExistsMC)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)

pieceOffsetsMC :: MoveConstraint ()
pieceOffsetsMC = do
  piece <- pieceExistsMC
  Move from to <- lift ask
  unless (to `elem` applyPieceOffsets (getPieceOffsets piece) from) (errorMC PieceTypeDestError)
