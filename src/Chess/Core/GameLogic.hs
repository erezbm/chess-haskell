module Chess.Core.GameLogic where

import Chess.Core.Models
import Chess.Core.MoveConstraints.Legal
import Data.Functor ((<&>))
import Utils

tryMakeMove :: GameState -> Move -> Maybe GameState
tryMakeMove gameState move@(Move source dest) =
  let isLegal = not . null $ legalMC gameState source [dest]
   in boolToMaybe isLegal $ makeMove gameState move

makeMove :: GameState -> Move -> GameState
makeMove gameState move@(Move from to) =
  -- TODO: handle castling, clear the eaten pawn in enpassant, pawn promotion
  --   update the other fields of gamestate
  case moveType gameState move of
    BasicMove ->
      let piece = getPiece from (board gameState)
          newBoard =
            setPiece Nothing from $
              setPiece piece to $
                board gameState
       in gameState{turn = nextTurn gameState, board = newBoard}
    Castling -> undefined
    EnPassant -> undefined
    Promotion -> undefined
 where
  moveType :: GameState -> Move -> MoveType
  moveType gameState move@(Move from to)
    | move `elem` castlingMoves = Castling
    | isPawnMove && (squareRank to `elem` [minBound, maxBound]) = Promotion
    | isPawnMove && enPassantDestSquare gameState == Just to = EnPassant
    | otherwise = BasicMove
   where
    -- e1g1, e1c1, e8g8, e8c8
    castlingMoves = [Move (rank, eFile) (rank, destFile) | destFile <- [gFile, cFile], rank <- [mkRank 0, mkRank 7]]
    eFile = mkFile 4
    cFile = mkFile 2
    gFile = mkFile 6

    isPawnMove = (pieceType <$> getPiece from (board gameState)) == Just Pawn

data MoveType = BasicMove | Castling | EnPassant | Promotion

enPassantDestSquare :: GameState -> Maybe Square
enPassantDestSquare gameState = lastPawnLeap gameState <&> mkSquare (mkRank (if turn gameState == White then 5 else 2))
