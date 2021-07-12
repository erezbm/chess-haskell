{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chess.Core.Models.Square where

import Control.Applicative
import Data.Array
import Data.Maybe
import Utils

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

mkMbRank :: Int -> Maybe Rank
mkMbRank = filterMaybe (inRange (minBound, maxBound)) . Rank

mkMbFile :: Int -> Maybe File
mkMbFile = filterMaybe (inRange (minBound, maxBound)) . File

mkRank :: Int -> Rank
mkRank = fromJust . mkMbRank

mkFile :: Int -> File
mkFile = fromJust . mkMbFile

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

allSquares :: [Square]
allSquares = liftA2 mkSquare allRanks allFiles