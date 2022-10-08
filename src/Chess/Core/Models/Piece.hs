module Chess.Core.Models.Piece where

import Chess.Core.Models.Player

data PieceType = Pawn | Bishop | Rook | Knight | Queen | King deriving (Show, Eq)

data Piece = Piece {piecePlayer :: Player, pieceType :: PieceType} deriving (Show, Eq)
