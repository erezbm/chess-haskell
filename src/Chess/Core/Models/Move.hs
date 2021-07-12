module Chess.Core.Models.Move where

import Chess.Core.Models.Square

data Move = Move { moveSource :: Square, moveDest :: Square } deriving (Show)
