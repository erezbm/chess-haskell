module Chess.Core.MoveConstraint where

import Chess.Core.Models.Square

type MoveConstraint = [Square] -> [Square]
