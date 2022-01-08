module Chess.Core.Models.Board where

import Chess.Core.Models.Piece
import Chess.Core.Models.Player
import Chess.Core.Models.Square
import Data.Array

{-# ANN Board ("HLint: ignore Use newtype instead of data" :: String) #-}
data Board = Board (Array Square (Maybe Piece)) deriving (Show)

getRankPieces :: Board -> Rank -> [(Square, Maybe Piece)]
getRankPieces (Board a) rank = filter ((== rank) . squareRank . fst) $ assocs a

getFilePieces :: Board -> File -> [(Square, Maybe Piece)]
getFilePieces (Board a) file = filter ((== file) . squareFile . fst) $ assocs a

getPiece :: Square -> Board -> Maybe Piece
getPiece square (Board a) = a ! square

setPiece :: Maybe Piece -> Square -> Board -> Board
setPiece piece square (Board a) = Board (a // [(square, piece)])

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
