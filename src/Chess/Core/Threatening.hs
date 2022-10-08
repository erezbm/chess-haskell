module Chess.Core.Threatening where

import Chess.Core.Models
import Control.Monad
import Data.Function
import Data.Maybe
import Utils

threatenedSquares :: GameState -> [Square]
threatenedSquares gameState = getOpponentPieces gameState >>= pieceThreatenedSquares (board gameState) . fst

pieceThreatenedSquares :: Board -> Square -> [Square]
pieceThreatenedSquares board source = do
  piece <- getPiece board source & maybeToList
  let PieceOffsets squareOffsets repeatability = getPieceBasicAttackOffsets piece
  offset <- squareOffsets
  let squares = generateList (applySquareOffset offset) source
  let isEmpty square = isNothing $ getPiece board square
  let empties = takeWhile isEmpty squares
  let n = if repeatability == Many then length empties else 0
  let squares' = take (n + 1) squares
  let lastMbPiece = safeLast squares' >>= getPiece board
  let takeLast = case lastMbPiece of
        Just (Piece player _) | player /= opponent (piecePlayer piece) -> 0
        _ -> 1

  take (n + takeLast) squares

getOpponentPieces :: GameState -> [(Square, Piece)]
getOpponentPieces gameState = do
  (square, mbPiece) <- getAllPieces (board gameState)
  piece <- maybeToList mbPiece
  guard $ piecePlayer piece == opponent (turn gameState)
  return (square, piece)

opponentInCheck :: GameState -> Bool
opponentInCheck gameState = any ((== Just opponentKing) . getPiece (board gameState)) $ threatenedSquares gameState
 where
  opponentKing = Piece (opponent $ turn gameState) King
