module Chess.Core.MoveConstraints.LeapOver (leapOverMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint (IllegalMoveError (PathBlockedError), MoveConstraint, errorMC)
import Chess.Core.MoveConstraints.PieceExists (pieceExistsMC)
import Control.Monad (mfilter, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (isNothing)
import Utils (generateList)

leapOverMC :: MoveConstraint ()
leapOverMC = do
  piece <- pieceExistsMC
  let PieceOffsets squareOffsets repeatability = getPieceOffsets piece
  gameState <- ask
  Move source to <- lift ask
  when (repeatability == Many) $
    unless (to `elem` validDests gameState squareOffsets source) (errorMC PathBlockedError)
 where
  validDests :: GameState -> [SquareOffset] -> Square -> [Square]
  validDests gameState squareOffsets source = do
    let isEmpty square = isNothing $ getPiece square (board gameState)
    offset <- squareOffsets
    let n = length $ generateList (mfilter isEmpty . applySquareOffset offset) source
    let squares = generateList (applySquareOffset offset) source
    take (n + 1) squares
