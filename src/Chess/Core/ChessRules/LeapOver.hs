module Chess.Core.ChessRules.LeapOver (leapOverCR) where

import Chess.Core.Models
import Chess.Core.ChessRule (IllegalMoveError (PathBlockedError), ChessRule, askGameState, askMove, errorCR)
import Chess.Core.ChessRules.PieceExists (pieceExistsCR)
import Control.Monad (mfilter, unless, when)
import Data.Maybe (isNothing)
import Utils (generateList)

leapOverCR :: ChessRule ()
leapOverCR = do
  piece <- pieceExistsCR
  let PieceOffsets squareOffsets repeatability = getPieceOffsets piece
  gameState <- askGameState
  Move source to <- askMove
  when (repeatability == Many) $
    unless (to `elem` validDests gameState squareOffsets source) (errorCR PathBlockedError)
 where
  validDests :: GameState -> [SquareOffset] -> Square -> [Square]
  validDests gameState squareOffsets source = do
    let isEmpty square = isNothing $ getPiece square (board gameState)
    offset <- squareOffsets
    let n = length $ generateList (mfilter isEmpty . applySquareOffset offset) source
    let squares = generateList (applySquareOffset offset) source
    take (n + 1) squares
