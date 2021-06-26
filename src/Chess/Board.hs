module Chess.Board where

import Chess.Player
import Chess.Square
import Data.Array

data PieceType = Pawn | Bishop | Rook | Knight | Queen | King deriving (Show)

data Piece = Piece {piecePlayer :: Player, pieceType :: PieceType} deriving (Show)

{-# ANN Board ("HLint: ignore Use newtype instead of data" :: String) #-}
data Board = Board (Array Square (Maybe Piece)) deriving (Show)

getRankPieces :: Board -> Rank -> [(Square, Maybe Piece)]
getRankPieces (Board a) rank = filter ((== rank) . squareRank . fst) $ assocs a

getFilePieces :: Board -> File -> [(Square, Maybe Piece)]
getFilePieces (Board a) file = filter ((== file) . squareFile . fst) $ assocs a

initialBoard :: Board
initialBoard = Board $ array (minBound, maxBound) positions
 where
  positions :: [(Square, Maybe Piece)]
  positions = do
    (rankPieces, rank) <- zip ranksPieces allRanks
    (piece, file) <- zip rankPieces allFiles
    return (mkSquare rank file, piece)

  ranksPieces :: [[Maybe Piece]]
  ranksPieces =
    [edgeRankPieces White, nearEdgeRankPieces White]
      ++ replicate (length allRanks - 4) emptyRankPieces
      ++ [nearEdgeRankPieces Black, edgeRankPieces Black]

  edgeRankPieces, nearEdgeRankPieces :: Player -> [Maybe Piece]
  edgeRankPieces player = map (Just . Piece player) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
  nearEdgeRankPieces player = replicate (length allFiles) $ Just (Piece player Pawn)

  emptyRankPieces :: [Maybe Piece]
  emptyRankPieces = replicate (length allFiles) Nothing
