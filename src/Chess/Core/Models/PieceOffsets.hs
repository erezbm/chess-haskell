module Chess.Core.Models.PieceOffsets where

import Chess.Core.Models.Board
import Chess.Core.Models.Piece
import Chess.Core.Models.Player
import Chess.Core.Models.Square
import Utils

data SquareOffset = SquareOffset {rankOffset :: Int, fileOffset :: Int}

data Repeatability = Finite Int | Infinite deriving (Eq)

once :: Repeatability
once = Finite 1

data PieceOffsets = PieceOffsets {pieceSquareOffsets :: [SquareOffset], repeatability :: Repeatability}

pawnAttackSquareOffsets, pawnBasicMovementSquareOffsets :: Player -> [SquareOffset]
pawnAttackSquareOffsets player = [SquareOffset (playerDirection player) f | f <- [1, -1]]
pawnBasicMovementSquareOffsets player = [SquareOffset (playerDirection player) 0]

getPieceBasicAttackOffsets :: Piece -> PieceOffsets
getPieceBasicAttackOffsets (Piece player Pawn) = PieceOffsets{pieceSquareOffsets = pawnAttackSquareOffsets player, repeatability = once}
getPieceBasicAttackOffsets piece = getPieceBasicOffsets piece

getPieceBasicMovementOffsets :: Piece -> PieceOffsets
getPieceBasicMovementOffsets (Piece player Pawn) = PieceOffsets{pieceSquareOffsets = pawnBasicMovementSquareOffsets player, repeatability = once}
getPieceBasicMovementOffsets piece = getPieceBasicOffsets piece

getPieceBasicOffsets :: Piece -> PieceOffsets
getPieceBasicOffsets (Piece player Pawn) = PieceOffsets{pieceSquareOffsets = pawnBasicMovementSquareOffsets player ++ pawnAttackSquareOffsets player, repeatability = once}
getPieceBasicOffsets (Piece _ pieceType) = getPieceBasicOffsets' pieceType
 where
  getPieceBasicOffsets' Bishop = PieceOffsets{pieceSquareOffsets = diagonalOffsets, repeatability = Infinite}
  getPieceBasicOffsets' Rook = PieceOffsets{pieceSquareOffsets = cardinalOffsets, repeatability = Infinite}
  getPieceBasicOffsets' Knight = PieceOffsets{pieceSquareOffsets = knightOffsets, repeatability = once}
  getPieceBasicOffsets' Queen = PieceOffsets{pieceSquareOffsets = cardinalOffsets ++ diagonalOffsets, repeatability = Infinite}
  getPieceBasicOffsets' King = PieceOffsets{pieceSquareOffsets = cardinalOffsets ++ diagonalOffsets, repeatability = once}
  getPieceBasicOffsets' Pawn = error "Pawn should be matched earlier"

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

applySquareOffset :: SquareOffset -> Square -> Maybe Square
applySquareOffset (SquareOffset rankOffset fileOffset) (rank, file) = do
  newRank <- mkMbRank $ rankIndex rank + rankOffset
  newFile <- mkMbFile $ fileIndex file + fileOffset
  return $ mkSquare newRank newFile

-- TODO: change name and take predicate to stop
applyPieceOffsets :: PieceOffsets -> Square -> [Square]
applyPieceOffsets (PieceOffsets squareOffsets repeatability) source = do
  offset <- squareOffsets
  let squares = generateList (applySquareOffset offset) source
  case repeatability of
    Infinite -> squares
    Finite n -> take n squares
