module Main where

import Chess.Core.GameLoop (gameLoop)
import Chess.UI.Terminal.TerminalUI (TerminalUI (TerminalUI))
import Chess.UI.Terminal.UI ()
import System.Environment (getArgs)

main :: IO ()
main = do
  isAscii <- any (`elem` ["--ascii", "--no-unicode"]) <$> getArgs
  gameLoop $ TerminalUI isAscii 2

-- printAllColors = do
--   for_ [0..255] $ \i -> putStr (show i) >> putChunk (back (color256 i) " ")
--   putStrLn ""
--   for_ [0..255] $ \i -> putStr (show i) >> putChunk (fore (color256 i) "A")
--   putStrLn ""
