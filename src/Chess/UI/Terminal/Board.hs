module Chess.UI.Terminal.Board where

import Chess.Core.Models
import Chess.UI.Terminal.Piece
import Data.Foldable
import Data.List
import Data.String
import Rainbow

data TileColor = LightTile | DarkTile
data TileDisplay = TileDisplay {tileColor :: TileColor, mbPiece :: Maybe Piece}

type TileChunks = [Chunk]
newtype RankChunks = RankChunks [[Chunk]]

tileToChunks :: Bool -> Int -> TileDisplay -> TileChunks
tileToChunks isAscii size (TileDisplay tileColor mbPiece) =
  let rows = 2 * size - 1
      columns = 2 * rows
      background = back $ tileColorRadiant tileColor
      emptyChunk = background $ fromString (replicate columns ' ')
      emptyChunks = replicate (rows `div` 2) emptyChunk
      pieceChunkPrefix = fromString (replicate ((columns - 2) `div` 2) ' ')
      pieceChunkPostfix = fromString (replicate (columns `div` 2) ' ')
      pieceChunk = background $ pieceChunkPrefix <> mbPieceChunk isAscii mbPiece <> pieceChunkPostfix
   in emptyChunks ++ [pieceChunk] ++ emptyChunks

tilesToRankChunks :: [TileChunks] -> RankChunks
tilesToRankChunks = RankChunks . transpose

displayRankChunks :: RankChunks -> IO ()
displayRankChunks (RankChunks chunks) = traverse_ putChunksLn chunks

displayBoard :: Bool -> Int -> Board -> IO ()
displayBoard isAscii size board = for_ (reverse allRanks) displayRank
 where
  displayRank rank = displayRankChunks $ tilesToRankChunks tileChunks
   where
    rowChunks = map (\(square, mbPiece) -> TileDisplay (squareTileColor square) mbPiece) (getRankPieces board rank)
    tileChunks = map (tileToChunks isAscii size) rowChunks

squareTileColor :: Square -> TileColor
squareTileColor (Rank rankIndex, File fileIndex)
  | rankIndex `mod` 2 /= fileIndex `mod` 2 = LightTile
  | otherwise = DarkTile

rankTileColors :: Rank -> [TileColor]
rankTileColors rank = map (squareTileColor . mkSquare rank) allFiles

-- Tamir
----------------------------------------------------------------------------------------------------------------------
-- Erez

displayBoard' :: Bool -> Int -> Board -> IO ()
displayBoard' isAscii size board = for_ (reverse allRanks) displayRank
 where
  displayRank :: Rank -> IO ()
  displayRank rank = for_ (emptyRowChunks ++ [rowChunks True] ++ emptyRowChunks) putChunksLn
   where
    rankTiles = map tileColorRadiant (rankTileColors rank) `zip` rankToPieceChunks rank
    rows = 2 * size - 1
    columns = 2 * rows
    prefixChunk = fromString $ replicate ((columns - 2) `div` 2) ' '
    postfixChunk = fromString $ replicate (columns `div` 2) ' '
    tileRowChunk isMiddle pieceChunk = prefixChunk <> (if isMiddle then pieceChunk else " ") <> postfixChunk
    rowChunks isMiddle = map (\(tileRadiant, pieceChunk) -> back tileRadiant $ tileRowChunk isMiddle pieceChunk) rankTiles
    emptyRowChunks = replicate (rows `div` 2) (rowChunks False)
  rankToPieceChunks :: Rank -> [Chunk]
  rankToPieceChunks rank = map (mbPieceChunk isAscii . snd) $ getRankPieces board rank

-- Much research has been done!
tileColorRadiant :: TileColor -> Radiant
tileColorRadiant LightTile = color256 230 -- 187 also good
tileColorRadiant DarkTile = color256 107
