module Chess.Core.Piece where

import Chess.Core.Player

data PieceType = Pawn | Bishop | Rook | Knight | Queen | King deriving (Show)

data Piece = Piece {piecePlayer :: Player, pieceType :: PieceType} deriving (Show)
