module Chess.UI.Terminal.Player where

import Chess.Core.Player
import Rainbow

playerRadiant :: Player -> Radiant
playerRadiant White = blue
playerRadiant Black = black
