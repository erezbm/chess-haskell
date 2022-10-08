module Chess.Core.ChessRules.Castling where

import Chess.Core.ChessRule
import Chess.Core.ChessRules.Turn
import Chess.Core.Models
import Chess.Core.Threatening
import Control.Monad
import Data.Function
import Data.Maybe
import Utils
import Utils.Point

castlingCR :: ChessRule ()
castlingCR = do
  gameState <- askGameState
  move@(Move from to) <- askMove
  let castlingSide = fromMaybe (error "castlingCR: Irrelevant") (isCastlingRelevant gameState move)

  (_, player) <- turnCR
  let eligiblity = getPlayerState gameState player
  let isEligible = eligiblity `elem` [BothEligible, castlingSideEligiblity castlingSide]

  let kingPath = rangeAbs (from, to)
  let betweenKingAndRookPath = init $ tail $ rangeAbs (from, initialRookSquare player castlingSide)
  let isPathEmpty = all (isNothing . getPiece (board gameState)) betweenKingAndRookPath
  let isThreatened = any (`elem` threatenedSquares gameState) kingPath

  let isLegal = isEligible && isPathEmpty && not isThreatened

  unless isLegal (errorCR CastlingError)

castlingSideEligiblity :: BoardSide -> CastlingEligiblity
castlingSideEligiblity KingSide = KingSideEligible
castlingSideEligiblity QueenSide = QueenSideEligible

isCastlingRelevant :: GameState -> Move -> Maybe BoardSide
isCastlingRelevant gameState (Move from to) = do
  piece <- getPiece (board gameState) from
  guard $ pieceType piece == King && from == initialKingSquare (piecePlayer piece)
  let offset = ((-) `on` squareToPoint) to from
  case offset of
    Point 0 2 -> Just KingSide
    Point 0 (-2) -> Just QueenSide
    _ -> Nothing
