module Chess.Core.ChessRules.PathBlocked (pathBlockedCR) where

import Chess.Core.ChessRule
import Chess.Core.ChessRules.PieceExists
import Chess.Core.Models
import Chess.Core.Threatening
import Control.Monad

pathBlockedCR :: ChessRule ()
pathBlockedCR = do
  -- TODO: add dependecy on (basicMove <|> castling <|> pawnLeap)
  _ <- pieceExistsCR
  gameState <- askGameState
  Move source to <- askMove
  unless (to `elem` pieceThreatenedSquares (board gameState) source) (errorCR PathBlockedError)
