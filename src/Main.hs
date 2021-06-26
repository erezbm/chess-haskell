module Main where

import Chess.Core.GameState
import Chess.UI.Terminal.Board

main :: IO ()
main = displayBoard' 2 $ board initialGameState
