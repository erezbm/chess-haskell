{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Square where

import Chess.Player
import Data.Array

-- Rank (aka row) 1, File (aka column) 1 is the white rook on the white queens side
-- https://cdn.discordapp.com/attachments/518448953512165387/857713363139821608/unknown.png

newtype Rank = Rank Int deriving (Eq, Ord, Enum, Ix, Show)

newtype File = File Int deriving (Eq, Ord, Enum, Ix, Show)

instance Bounded Rank where
  minBound = mkRank 0
  maxBound = mkRank 7

instance Bounded File where
  minBound = mkFile 0
  maxBound = mkFile 7

type Square = (Rank, File)

mkRank :: Int -> Rank
mkRank n
  | 0 <= n && n <= 7 = Rank n
  | otherwise = error "mkRank: out of bounds"

mkFile :: Int -> File
mkFile n
  | 0 <= n && n <= 7 = File n
  | otherwise = error "mkFile: out of bounds"

mkSquare :: Rank -> File -> Square
mkSquare = (,)

allRanks :: [Rank]
allRanks = [mkRank 0 .. mkRank 7]

allFiles :: [File]
allFiles = [mkFile 0 .. mkFile 7]

rankToPlayer :: Rank -> Player
rankToPlayer (Rank rankIndex)
  | rankIndex <= 3 = White
  | otherwise = Black
