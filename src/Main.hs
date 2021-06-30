module Main where

import Chess.UI.Terminal.GameLoop
import System.Environment (getArgs)

main :: IO ()
main = do
  isAscii <- any (`elem` ["--ascii", "--no-unicode"]) <$> getArgs
  gameLoop isAscii 2

-- printAllColors = do
--   for_ [0..255] $ \i -> putStr (show i) >> putChunk (back (color256 i) " ")
--   putStrLn ""
--   for_ [0..255] $ \i -> putStr (show i) >> putChunk (fore (color256 i) "A")
--   putStrLn ""
