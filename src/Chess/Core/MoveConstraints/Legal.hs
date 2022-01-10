module Chess.Core.MoveConstraints.Legal (legalMC) where

import Chess.Core.MoveConstraint
import Chess.Core.MoveConstraints.LeapOver
import Chess.Core.MoveConstraints.PieceOffsets
import Chess.Core.MoveConstraints.Turn

legalMC :: MoveConstraint ()
legalMC = sequence_ [turnMC, pieceOffsetsMC, leapOverMC]
-- TODO: add "The occupancy of the target square in conjunction with its piece color (if any piece)",
--           En passant stuff
--           Castling stuff
--           leaves king in check (pseudo-legal -> legal)
