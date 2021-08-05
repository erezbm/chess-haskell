module Chess.Core.MoveConstraints.Turn (turnMC) where

import Chess.Core.Models
import Chess.Core.MoveConstraint
import Data.Bool

turnMC :: Player -> Player -> MoveConstraint
turnMC turn movingPiecePlayer = bool (const []) id $ turn == movingPiecePlayer
