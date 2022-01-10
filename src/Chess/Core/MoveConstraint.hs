module Chess.Core.MoveConstraint where

import Chess.Core.Models (GameState, Move)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Functor (($>))

type MoveConstraint a = ReaderT GameState (ReaderT Move (Except IllegalMoveError)) a

runMoveConstraint :: MoveConstraint a -> GameState -> Move -> Either IllegalMoveError ()
runMoveConstraint mc gameState move = runExcept (runReaderT (runReaderT mc gameState) move) $> ()

data IllegalMoveError
  = EmptySquareError
  | OpponentPieceError
  | PathBlockedError
  | PieceTypeDestError
  deriving (Show)

errorMC :: IllegalMoveError -> MoveConstraint a
errorMC = lift . lift . throwE
