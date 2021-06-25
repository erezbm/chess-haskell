module Chess.TextUI.Board where

import Chess.Board
import Chess.Player
import Chess.Square
import Data.Foldable
import Data.List
import Data.String
import Rainbow

data TileColor = LightTile | DarkTile
data TileDisplay = TileDisplay {tileColor :: TileColor, mbPiece :: Maybe Piece}

type TileChunks = [Chunk]
newtype RankChunks = RankChunks [[Chunk]]

tileToChunks :: Int -> TileDisplay -> TileChunks
tileToChunks size (TileDisplay tileColor mbPiece) =
  let rows = 2 * size - 1
      columns = 2 * rows
      background = back $ tileColorToRadiant tileColor
      foreground = maybe id (fore . playerToRadiant . piecePlayer) mbPiece
      emptyChunk = background $ fromString (replicate columns ' ')
      s = replicate ((columns - 2) `div` 2) ' ' ++ maybePieceString mbPiece ++ replicate (columns `div` 2) ' '
      pieceChunk = background $ foreground $ fromString s
      emptyChunks = replicate (rows `div` 2) emptyChunk
   in emptyChunks ++ [pieceChunk] ++ emptyChunks

tilesToRankChunks :: [TileChunks] -> RankChunks
tilesToRankChunks = RankChunks . transpose

displayRankChunks :: RankChunks -> IO ()
displayRankChunks (RankChunks chunks) = traverse_ putChunksLn chunks

displayBoard' :: Int -> Board -> IO ()
displayBoard' size board = for_ (reverse allRanks) displayRank'
 where
  displayRank' rank = displayRankChunks $ tilesToRankChunks tileChunks
   where
    rowChunks = map (\(square, mbPiece) -> TileDisplay (squareToTileColor square) mbPiece) (getRankPieces board rank)
    tileChunks = map (tileToChunks size) rowChunks

squareToTileColor :: Square -> TileColor
squareToTileColor (Rank rankIndex, File fileIndex)
  | rankIndex `mod` 2 /= fileIndex `mod` 2 = LightTile
  | otherwise = DarkTile

-- Tamir
----------------------------------------------------------------------------------------------------------------------
-- Erez

displayBoard :: Int -> Board -> IO ()
displayBoard size board = for_ (reverse allRanks) displayRank
 where
  displayRank :: Rank -> IO ()
  displayRank rank@(Rank rankIndex) = do
    let rankTiles = zip (map tileColorToRadiant $ drop rankIndex $ cycle [LightTile, DarkTile]) (rankPieceStrings rank)
    let rows = 2 * size - 1
    let columns = 2 * rows
    let foreground = fore (playerToRadiant $ rankToPlayer rank)
    let rowString isMiddle tileContent = fromString $ replicate ((columns - 2) `div` 2) ' ' ++ (if isMiddle then tileContent else " ") ++ replicate (columns `div` 2) ' '
    let rowChunks isMiddle = map (\(tileRadiant, tileContent) -> foreground $ back tileRadiant (rowString isMiddle tileContent)) rankTiles
    let empties = replicate (rows `div` 2) (rowChunks False)
    for_ (empties ++ [rowChunks True] ++ empties) putChunksLn
  rankPieceStrings :: Rank -> [String]
  rankPieceStrings rank = map (maybePieceString . snd) $ getRankPieces board rank

maybePieceString :: Maybe Piece -> String
maybePieceString = maybe " " pieceString

pieceString :: Piece -> String
pieceString (Piece Black Pawn) = "♙" -- "♟︎"
pieceString (Piece Black Bishop) = "♝"
pieceString (Piece Black Rook) = "♜"
pieceString (Piece Black Knight) = "♞"
pieceString (Piece Black Queen) = "♛"
pieceString (Piece Black King) = "♚"
pieceString (Piece White Pawn) = "♙"
pieceString (Piece White Bishop) = "♗"
pieceString (Piece White Rook) = "♖"
pieceString (Piece White Knight) = "♘"
pieceString (Piece White Queen) = "♕"
pieceString (Piece White King) = "♔"

-- Much research has been done!
tileColorToRadiant :: TileColor -> Radiant
tileColorToRadiant LightTile = color256 230 -- 187 also good
tileColorToRadiant DarkTile = color256 107

playerToRadiant :: Player -> Radiant
playerToRadiant White = white
playerToRadiant Black = black
