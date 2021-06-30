module Chess.UI.Terminal.Piece (mbPieceChunk) where

import Chess.Core.Piece
import Chess.Core.Player
import Chess.UI.Terminal.Player
import Data.Char
import Data.Function
import Data.String
import Rainbow

mbPieceChunk :: Bool -> Maybe Piece -> Chunk
mbPieceChunk isAscii = maybe " " (pieceChunk isAscii)

pieceChunk :: Bool -> Piece -> Chunk
pieceChunk isAscii (Piece player pieceType) = fromString (pieceString isAscii) & fore (playerRadiant player)
 where
  pieceString :: Bool -> String
  pieceString True = asciiPieceString
  pieceString False = unicodePieceString

  -- Ascii
  asciiPieceString :: String
  asciiPieceString = playerCasing player $ asciiPieceString' pieceType

  playerCasing Black = map toLower
  playerCasing White = map toUpper

  asciiPieceString' Pawn = "P"
  asciiPieceString' Bishop = "B"
  asciiPieceString' Rook = "R"
  asciiPieceString' Knight = "N"
  asciiPieceString' Queen = "Q"
  asciiPieceString' King = "K"

  -- Unicode
  unicodePieceString :: String
  unicodePieceString = playerVersion player pieceType

  playerVersion Black = blackUnicodePieceString
  playerVersion White = whiteUnicodePieceString

  blackUnicodePieceString Pawn = "♙" -- "♟︎" (The unicode black pawn is bigger on some terminals)
  blackUnicodePieceString Bishop = "♝"
  blackUnicodePieceString Rook = "♜"
  blackUnicodePieceString Knight = "♞"
  blackUnicodePieceString Queen = "♛"
  blackUnicodePieceString King = "♚"

  whiteUnicodePieceString Pawn = "♙"
  whiteUnicodePieceString Bishop = "♗"
  whiteUnicodePieceString Rook = "♖"
  whiteUnicodePieceString Knight = "♘"
  whiteUnicodePieceString Queen = "♕"
  whiteUnicodePieceString King = "♔"
