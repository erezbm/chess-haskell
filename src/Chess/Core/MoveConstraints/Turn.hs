module Chess.Core.MoveConstraints.Turn (turnMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint (IllegalMoveError (OpponentPieceError), MoveConstraint, errorMC)
import Chess.Core.MoveConstraints.PieceExists (pieceExistsMC)
import Control.Monad (unless)
import Control.Monad.Trans.Reader (ask)

turnMC :: MoveConstraint ()
turnMC = do
  piece <- pieceExistsMC
  gameState <- ask
  unless (turn gameState == piecePlayer piece) (errorMC OpponentPieceError)
