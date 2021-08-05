module Chess.Core.MoveConstraints.LeapOver (leapOverMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint
import Data.List
import Data.Maybe
import Utils

leapOverMC :: Board -> Piece -> Square -> MoveConstraint
leapOverMC board piece@(Piece piecePlayer pieceType) source =
  let PieceOffsets squareOffsets repeatability = getPieceOffsets piece
      isEmpty square = isNothing $ getPiece square board
      go = flip intersect $ do
        offset <- squareOffsets
        let n = length $ generateList (find isEmpty . applySquareOffset offset) source
        let squares = generateList (applySquareOffset offset) source
        take (n + 1) squares
   in leapOver repeatability go

leapOver :: Repeatability -> MoveConstraint -> MoveConstraint
leapOver Many mc = mc
leapOver Once _ = id
