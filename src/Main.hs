module Main where

import Chess.GameState
import Chess.TextUI.Board

main :: IO ()
main = displayBoard' 2 $ board initialGameState
