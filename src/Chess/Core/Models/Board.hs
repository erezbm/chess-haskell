module Chess.Core.Models.Board where

import Chess.Core.Models.Piece
import Chess.Core.Models.Player
import Chess.Core.Models.Square
import Data.Array
import Data.Coerce

newtype Board = Board (Array Square (Maybe Piece)) deriving (Show)

data BoardSide = QueenSide | KingSide

getAllPieces :: Board -> [(Square, Maybe Piece)]
getAllPieces = assocs . coerce

getRankPieces :: Board -> Rank -> [(Square, Maybe Piece)]
getRankPieces (Board a) rank = filter ((== rank) . squareRank . fst) $ assocs a

getFilePieces :: Board -> File -> [(Square, Maybe Piece)]
getFilePieces (Board a) file = filter ((== file) . squareFile . fst) $ assocs a

getPiece :: Board -> Square -> Maybe Piece
getPiece = (!) . coerce

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

playerDirection :: Num a => Player -> a
playerDirection White = 1
playerDirection Black = -1

initialKingSquare :: Player -> Square
initialKingSquare White = mkSquare minBound (mkFile 4)
initialKingSquare Black = mkSquare maxBound (mkFile 4)

initialRookSquare :: Player -> BoardSide -> Square
initialRookSquare White KingSide = mkSquare minBound maxBound
initialRookSquare White QueenSide = mkSquare minBound minBound
initialRookSquare Black KingSide = mkSquare maxBound maxBound
initialRookSquare Black QueenSide = mkSquare maxBound minBound
