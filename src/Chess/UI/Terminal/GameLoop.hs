{-# LANGUAGE TupleSections #-}

module Chess.UI.Terminal.GameLoop where

import Chess.Core.GameLogic
import Chess.Core.Models
import Chess.UI.Terminal.Board
import Control.Applicative (liftA2, many)
import Parsing.Combinator (anyChar, eof, space)
import Parsing.Parser (Parser, parse)
import System.IO (hFlush, stdout)
import Utils (fmapMaybe)

gameLoop :: Bool -> Int -> IO ()
gameLoop isAscii size = loop initialGameState
 where
  loop gameState = do
    displayBoard isAscii size $ board gameState
    putStr "Enter move: "
    hFlush stdout
    mbMove <- parse lineParser <$> getLine
    case mbMove of
      Nothing -> putStrLn "Parse error (example move: \"a1 b3\")" >> loop gameState
      Just move ->
        case tryMakeMove gameState move of
          Just nextGameState -> loop nextGameState
          Nothing -> putStrLn "Illegal move" >> loop gameState

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
