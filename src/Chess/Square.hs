{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Square where

import Chess.Player
import Data.Array

-- Rank (aka row) 1, File (aka column) 1 is the white rook on the white queens side
-- https://cdn.discordapp.com/attachments/518448953512165387/857713363139821608/unknown.png

newtype Rank = Rank {rankIndex :: Int} deriving (Eq, Ord, Enum, Ix, Show)

newtype File = File {fileIndex :: Int} deriving (Eq, Ord, Enum, Ix, Show)

instance Bounded Rank where
  minBound = Rank 0
  maxBound = Rank 7

instance Bounded File where
  minBound = File 0
  maxBound = File 7

type Square = (Rank, File)

mkRank :: Int -> Rank
mkRank index
  | minBound <= rank && rank <= maxBound = rank
  | otherwise = error "mkRank: out of bounds"
 where
  rank = Rank index

mkFile :: Int -> File
mkFile index
  | minBound <= file && file <= maxBound = file
  | otherwise = error "mkFile: out of bounds"
 where
  file = File index

mkSquare :: Rank -> File -> Square
mkSquare = (,)

squareRank :: Square -> Rank
squareRank = fst

squareFile :: Square -> File
squareFile = snd

allRanks :: [Rank]
allRanks = [minBound .. maxBound]

allFiles :: [File]
allFiles = [minBound .. maxBound]
