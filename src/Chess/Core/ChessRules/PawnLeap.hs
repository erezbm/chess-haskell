module Chess.Core.ChessRules.PawnLeap where

import Chess.Core.ChessRule
import Chess.Core.ChessRules.PieceExists
import Chess.Core.Models
import Control.Monad
import Data.Function
import Utils.Point

pawnLeapCR :: ChessRule ()
pawnLeapCR = do
  piece <- pieceExistsCR
  gameState <- askGameState
  move@(Move from _) <- askMove
  unless (isPawnLeapRelevant gameState move) $ error ("pawnLeapCR: Irrelevant: " ++ show piece)
  let isLegal = case piecePlayer piece of
        White -> squareRank from == succ minBound
        Black -> squareRank from == pred maxBound

  unless isLegal (errorCR PawnLeapError)

isPawnLeapRelevant :: GameState -> Move -> Bool
isPawnLeapRelevant gameState (Move from to) =
  let mbPiece = getPiece (board gameState) from
      offset = ((-) `on` squareToPoint) to from
      f piece = pieceType piece == Pawn && offset == Point (2 * playerDirection (piecePlayer piece)) 0
   in maybe False f mbPiece
