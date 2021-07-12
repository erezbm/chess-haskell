module Chess.Core.MoveConstraints.ValidMoves where

import Chess.Core.Models
import Chess.Core.MoveConstraint
import Data.Maybe
import Utils

validMovesMoveConstraint :: Square -> Piece -> MoveConstraint
validMovesMoveConstraint source piece = filter (`elem` validDests source piece)

validDests :: Square -> Piece -> [Square]
validDests source piece = applyPieceOffsets (getPieceMoves piece) source

applyPieceOffsets :: PieceOffsets -> Square -> [Square]
applyPieceOffsets (PieceOffsets squareOffsets Once) source = catMaybes (($ source) . applySquareOffset <$> squareOffsets)
applyPieceOffsets (PieceOffsets squareOffsets Many) source = do
  offset <- squareOffsets
  generateList (applySquareOffset offset) source

getPieceMoves :: Piece -> PieceOffsets
getPieceMoves (Piece player Pawn) =
  PieceOffsets
    { pieceSquareOffsets =
        [ SquareOffset (1 * factor) 0
        , SquareOffset (2 * factor) 0
        , SquareOffset (1 * factor) 1
        , SquareOffset (1 * factor) (-1)
        ]
    , repeatability = Once
    }
 where
  factor = playerFactor player
  playerFactor White = 1
  playerFactor Black = -1
getPieceMoves (Piece _ pieceType) = getPieceMoves' pieceType
 where
  getPieceMoves' Bishop = PieceOffsets{pieceSquareOffsets = diagonalOffsets, repeatability = Many}
  getPieceMoves' Rook = PieceOffsets{pieceSquareOffsets = cardinalOffsets, repeatability = Many}
  getPieceMoves' Knight = PieceOffsets{pieceSquareOffsets = knightOffsets, repeatability = Once}
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

data SquareOffset = SquareOffset {rankOffset :: Int, fileOffset :: Int}

data Repeatability = Once | Many

data PieceOffsets = PieceOffsets {pieceSquareOffsets :: [SquareOffset], repeatability :: Repeatability}

applySquareOffset :: SquareOffset -> Square -> Maybe Square
applySquareOffset (SquareOffset rankOffset fileOffset) (rank, file) = do
  newRank <- mkMbRank $ rankIndex rank + rankOffset
  newFile <- mkMbFile $ fileIndex file + fileOffset
  return $ mkSquare newRank newFile
