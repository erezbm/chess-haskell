module Chess.Core.ChessRules.Legal (legalCR) where

import Chess.Core.ChessRule
import Chess.Core.ChessRules.LeapOver
import Chess.Core.ChessRules.PieceOffsets
import Chess.Core.ChessRules.Turn

legalCR :: ChessRule ()
legalCR = sequence_ [turnCR, pieceOffsetsCR, leapOverCR]
-- TODO: add "The occupancy of the target square in conjunction with its piece color (if any piece)",
--           En passant stuff
--           Castling stuff
--           leaves king in check (pseudo-legal -> legal)
