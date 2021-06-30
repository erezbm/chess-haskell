module Chess.Core.GameLogic where

import Chess.Core.GameState
import Chess.Core.Move
import Chess.Core.Piece
import Chess.Core.Player

applyMove :: Move -> GameState -> Maybe GameState
applyMove move gameState = undefined

data SquareOffset = SquareOffset {rankOffset :: Int, fileOffset :: Int}

data Repeatability = Once | Many

data PieceOffsets = PieceOffsets {pieceSquareOffsets :: [SquareOffset], repeatability :: Repeatability}

getPieceMoves :: Piece -> PieceOffsets
getPieceMoves (Piece player Pawn) = PieceOffsets{pieceSquareOffsets = [SquareOffset (1 * factor player) 0, SquareOffset (2 * factor player) 0], repeatability = Once}
 where
  factor White = 1
  factor Black = -1
getPieceMoves (Piece _ pieceType) = getPieceMoves' pieceType
 where
  getPieceMoves' Bishop = PieceOffsets{pieceSquareOffsets = diagonalOffsets, repeatability = Many}
  getPieceMoves' Rook = PieceOffsets{pieceSquareOffsets = cardinalOffsets, repeatability = Many}
  getPieceMoves' Knight = PieceOffsets{pieceSquareOffsets = knightOffsets, repeatability = Many}
  getPieceMoves' Queen = PieceOffsets{pieceSquareOffsets = cardinalOffsets ++ diagonalOffsets, repeatability = Many}
  getPieceMoves' King = PieceOffsets{pieceSquareOffsets = cardinalOffsets ++ diagonalOffsets, repeatability = Once}
  getPieceMoves' Pawn = error "Pawn should be matched earlier"

  cardinalOffsets = [SquareOffset 1 0, SquareOffset 0 1, SquareOffset (-1) 0, SquareOffset 0 (-1)]
  diagonalOffsets = [SquareOffset 1 1, SquareOffset (-1) 1, SquareOffset (-1) (-1), SquareOffset 1 (-1)]
  knightOffsets =
    [ SquareOffset 2 1
    , SquareOffset 1 2
    , SquareOffset (-1) 2
    , SquareOffset (-2) 1
    , SquareOffset (-2) (-1)
    , SquareOffset (-1) (-2)
    , SquareOffset 1 (-2)
    , SquareOffset 2 (-1)
    ]
