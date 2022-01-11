module Chess.Core.ChessRule where

import Chess.Core.Models (GameState, Move)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Functor (($>))

type ChessRule a = ReaderT GameState (ReaderT Move (Except IllegalMoveError)) a

checkChessRule :: ChessRule a -> GameState -> Move -> Either IllegalMoveError ()
checkChessRule mc gameState move = runExcept (runReaderT (runReaderT mc gameState) move) $> ()

data IllegalMoveError
  = EmptySquareError
  | OpponentPieceError
  | PathBlockedError
  | PieceTypeDestError
  deriving (Show)

errorCR :: IllegalMoveError -> ChessRule a
errorCR = lift . lift . throwE

askGameState :: ChessRule GameState
askGameState = ask

askMove :: ChessRule Move
askMove = lift ask
