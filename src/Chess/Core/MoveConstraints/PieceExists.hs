module Chess.Core.MoveConstraints.PieceExists where

import Chess.Core.Models
import Chess.Core.MoveConstraint (IllegalMoveError (EmptySquareError), MoveConstraint, errorMC)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)

pieceExistsMC :: MoveConstraint Piece
pieceExistsMC = do
  gameState <- ask
  move <- lift ask
  maybe (errorMC EmptySquareError) return (getPiece (moveSource move) (board gameState))
