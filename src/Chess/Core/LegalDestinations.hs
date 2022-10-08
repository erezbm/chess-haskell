{-# LANGUAGE NamedFieldPuns #-}

module Chess.Core.LegalDestinations where

import Chess.Core.MakeMove
import Chess.Core.Models
import Chess.Core.Threatening
import Control.Monad
import Data.Function
import Data.Functor
import Data.Maybe

legalDestinations :: GameState -> Square -> [Square]
legalDestinations gameState@GameState{board} source = do
  -- must be from piece
  (Piece piecePlayer pieceType) <- getPiece board source & maybeToList
  -- must be from current player
  guard $ piecePlayer == turn gameState
  -- legal moves not considering check
  let pseudoLegalDests = case pieceType of
        Pawn -> filter pawnCanCapture threatenedSquares ++ pawnMoveDests
        King -> threatenedSquares ++ castlingDests
        _ -> threatenedSquares
  -- cannot make a move that results in self check (pseudo legal -> legal)
  filter (not . opponentInCheck . makeMove gameState . Move source) pseudoLegalDests
 where
  threatenedSquares :: [Square]
  threatenedSquares = pieceThreatenedSquares board source

  pawnCanCapture :: Square -> Bool
  pawnCanCapture d = isJust (getPiece board d) || enPassantDestSquare gameState == Just d

  pawnMoveDests :: [Square]
  pawnMoveDests = undefined

  castlingDests :: [Square]
  castlingDests = undefined

enPassantDestSquare :: GameState -> Maybe Square
enPassantDestSquare gameState = lastPawnLeap gameState <&> mkSquare (mkRank (if turn gameState == White then 5 else 2))
