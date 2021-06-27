module Main where

import Chess.Core.GameState
import Chess.UI.Terminal.Board
import System.Environment (getArgs)

main :: IO ()
main = do
  isAscii <- any (`elem` ["--ascii", "--no-unicode"]) <$> getArgs
  displayBoard' isAscii 2 $ board initialGameState

-- printAllColors = do
--   for_ [0..255] $ \i -> putStr (show i) >> putChunk (back (color256 i) " ")
--   putStrLn ""
--   for_ [0..255] $ \i -> putStr (show i) >> putChunk (fore (color256 i) "A")
--   putStrLn ""
