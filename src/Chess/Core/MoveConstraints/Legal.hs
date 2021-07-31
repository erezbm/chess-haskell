module Chess.Core.MoveConstraints.Legal (legalMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint
import Chess.Core.MoveConstraints.PieceOffsets
import Control.Monad
import Data.Function
import Data.Maybe

legalMC :: GameState -> Square -> MoveConstraint
legalMC gameState source dests = do
  piece <- getPiece source (board gameState) & maybeToList
  guard $ piecePlayer piece == turn gameState
  dests
    & pieceOffsetsMC piece source
    & undefined
