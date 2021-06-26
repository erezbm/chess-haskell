module Chess.Core.Player where

data Player = White | Black deriving (Show)

opponent :: Player -> Player
opponent White = Black
opponent Black = White
