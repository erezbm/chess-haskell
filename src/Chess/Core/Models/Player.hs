module Chess.Core.Models.Player where

data Player = White | Black deriving (Eq, Show)

opponent :: Player -> Player
opponent White = Black
opponent Black = White
