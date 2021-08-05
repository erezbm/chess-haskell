module Chess.Core.MoveConstraints.Legal (legalMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint
import Chess.Core.MoveConstraints.LeapOver
import Chess.Core.MoveConstraints.PieceOffsets
import Chess.Core.MoveConstraints.Turn
import Data.Function
import Data.Maybe

legalMC :: GameState -> Square -> MoveConstraint
legalMC gameState source dests = do
  piece <- getPiece source (board gameState) & maybeToList
  dests
    & turnMC (turn gameState) (piecePlayer piece)
    & pieceOffsetsMC piece source
    & leapOverMC (board gameState) piece source
    -- TODO: add "The occupancy of the target square in conjunction with its piece color (if any piece)",
    --           En passant stuff
    --           Castling stuff
    --           leaves king in check (pseudo-legal -> legal)
