module Chess.Core.MoveConstraints.PieceOffsets (pieceOffsetsMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint
import Data.List

pieceOffsetsMC :: Piece -> Square -> MoveConstraint
pieceOffsetsMC piece = flip intersect . applyPieceOffsets (getPieceOffsets piece)
