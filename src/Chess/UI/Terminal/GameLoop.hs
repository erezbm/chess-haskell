{-# LANGUAGE TupleSections #-}

module Chess.UI.Terminal.GameLoop where

import Chess.Core.GameLogic
import Chess.Core.GameState
import Chess.Core.Move
import Chess.Core.Square
import Chess.UI.Terminal.Board
import Data.Char
import System.IO

gameLoop :: Bool -> Int -> IO ()
gameLoop isAscii size = loop initialGameState
 where
  loop gameState = do
    displayBoard isAscii size $ board gameState
    putStr "Enter move: "
    hFlush stdout
    mbMove <- parseMove <$> getLine
    case mbMove of
      Nothing -> putStrLn "Invalid move" >> loop gameState
      Just (move, _) ->
        case applyMove move gameState of
          Just nextGameState -> loop nextGameState
          Nothing -> putStrLn "Invalid move" >> loop gameState

parseMove :: String -> Maybe (Move, String)
parseMove s = do
  (moveSource, s') <- parseSquare s
  (moveDest, s'') <- parseSquare (dropWhile isSpace s')
  return (Move moveSource moveDest, s'')

parseSquare :: String -> Maybe (Square, String)
parseSquare s = do
  (file, s') <- parseFile s
  (rank, s'') <- parseRank s'
  return (mkSquare rank file, s'')

parseRank :: String -> Maybe (Rank, String)
parseRank (c : s) = (,s) <$> lookup (digitToInt c) ([1 ..] `zip` allRanks)
parseRank _ = Nothing

parseFile :: String -> Maybe (File, String)
parseFile (c : s) = (,s) <$> lookup (toLower c) (['a' ..] `zip` allFiles)
parseFile _ = Nothing
