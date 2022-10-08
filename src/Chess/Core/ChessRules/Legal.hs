module Chess.Core.ChessRules.Legal (legalCR) where

import Chess.Core.ChessRule

import Chess.Core.ChessRules.BasicMove
import Chess.Core.ChessRules.Castling
import Chess.Core.ChessRules.PathBlocked
import Chess.Core.ChessRules.PawnLeap
import Chess.Core.ChessRules.Turn
import Data.Maybe

legalCR :: ChessRule ()
legalCR = do
  _ <- turnCR
  -- (piece, player) <- turnCR
  gameState <- askGameState
  move <- askMove
  case () of
    ()
      | isPawnLeapRelevant gameState move -> pawnLeapCR
      | isJust (isCastlingRelevant gameState move) -> castlingCR
      | otherwise -> basicMoveCR
  pathBlockedCR

-- TODO: add "The occupancy of the target square in conjunction with its piece color (if any piece)",
--           En passant stuff
--           leaves king in check (pseudo-legal -> legal)
