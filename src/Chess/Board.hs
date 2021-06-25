module Chess.Board where

import Chess.Player
import Chess.Square
import Data.Array

data PieceType = Pawn | Bishop | Rook | Knight | Queen | King deriving (Show)

data Piece = Piece {piecePlayer :: Player, pieceType :: PieceType} deriving (Show)

{-# ANN Board ("HLint: ignore Use newtype instead of data" :: String) #-}
data Board = Board (Array Square (Maybe Piece)) deriving (Show)

getRankPieces :: Board -> Rank -> [(Square, Maybe Piece)]
getRankPieces (Board a) rank = map (\square -> (square, a ! square)) $ range (mkSquare rank minBound, mkSquare rank maxBound)

getFilePieces :: Board -> File -> [(Square, Maybe Piece)]
getFilePieces (Board a) file = map (\square -> (square, a ! square)) $ range (mkSquare minBound file, mkSquare maxBound file)

initialBoard :: Board
initialBoard = Board $ array (bottomLeft, topRight) positions
 where
  bottomLeft = (mkRank 0, mkFile 0)
  topRight = (mkRank 7, mkFile 7)

  positions :: [(Square, Maybe Piece)]
  positions = do
    (rank, rankIndex) <- zip ranks (map mkRank [0 ..])
    (piece, fileIndex) <- zip rank (map mkFile [0 ..])
    return ((rankIndex, fileIndex), piece)

  ranks :: [[Maybe Piece]]
  ranks = [edgeRank White, nearEdgeRank White] ++ replicate 4 emptyRank ++ [nearEdgeRank Black, edgeRank Black]

  edgeRank, nearEdgeRank :: Player -> [Maybe Piece]
  edgeRank player = map (Just . Piece player) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
  nearEdgeRank player = replicate 8 $ Just (Piece player Pawn)

  emptyRank :: [Maybe Piece]
  emptyRank = replicate 8 Nothing
