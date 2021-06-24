module Board where

import Data.Array (Array, Ix, array)
import Player (Player (Black, White))

-- Rank (aka row) 1, File (aka column) 1 is the white rook on the white queens side
-- https://cdn.discordapp.com/attachments/518448953512165387/857713363139821608/unknown.png

newtype Rank = Rank Int deriving (Eq, Ord, Ix, Show)

newtype File = File Int deriving (Eq, Ord, Ix, Show)

type Tile = (Rank, File)

allTiles :: [Tile]
allTiles = []

mkRank :: Int -> Rank
mkRank n
  | 0 <= n && n <= 7 = Rank n
  | otherwise = error "mkRank: out of bounds"

mkFile :: Int -> File
mkFile n
  | 0 <= n && n <= 7 = File n
  | otherwise = error "mkFile: out of bounds"

data PieceType = Pawn | Bishop | Rook | Knight | Queen | King deriving (Show)

data Piece = Piece Player PieceType deriving (Show)

{-# ANN Board "HLint: ignore Use newtype instead of data" #-}
data Board = Board (Array Tile (Maybe Piece)) deriving (Show)

initialBoard :: Board
initialBoard = Board $ array (bottomLeft, topRight) positions
 where
  bottomLeft = (mkRank 0, mkFile 0)
  topRight = (mkRank 7, mkFile 7)

  positions :: [(Tile, Maybe Piece)]
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
