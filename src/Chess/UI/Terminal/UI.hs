{-# OPTIONS_GHC -Wno-orphans #-}

module Chess.UI.Terminal.UI where

import Chess.Core.GameLoop (UI (..))
import Chess.Core.Models
import Chess.UI.Terminal.Board (displayBoard)
import Chess.UI.Terminal.TerminalUI (TerminalUI)
import Control.Applicative (liftA2, many)
import Control.Monad (when)
import Parsing.Combinator (anyChar, eof, space)
import Parsing.Parser (Parser, parse)
import System.IO (hFlush, stdout)
import Utils (fmapMaybe)

instance UI TerminalUI where
  sendGameStarted = sendGameState
  requestMove _ _ = do
    putStr "Enter move: "
    hFlush stdout
    parse lineParser <$> getLine
  sendGameState ui p gameState = when (p == White) $ displayBoard ui (board gameState)
  sendRequestMoveFailedError _ _ = putStrLn "Parse error (example move: \"a1 b3\")"
  sendIllegalMoveError _ _ e = putStrLn ("Illegal move: " ++ show e)

lineParser :: Parser Move
lineParser = moveParser <* eof

moveParser :: Parser Move
moveParser = liftA2 Move squareParser (many space *> squareParser)

squareParser :: Parser Square
squareParser = liftA2 (flip mkSquare) fileParser rankParser

rankParser :: Parser Rank
rankParser = fmapMaybe (flip lookup $ ['1' ..] `zip` allRanks) anyChar

fileParser :: Parser File
fileParser = fmapMaybe (flip lookup $ ['a' ..] `zip` allFiles) anyChar
