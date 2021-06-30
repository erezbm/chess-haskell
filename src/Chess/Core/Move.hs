module Chess.Core.Move where

import Chess.Core.Square

data Move = Move { moveSource :: Square, moveDest :: Square } deriving (Show)
