{-# LANGUAGE NamedFieldPuns #-}

module Chess.Core.Models.GameState where

import Chess.Core.Models.Board
import Chess.Core.Models.Player
import Chess.Core.Models.Square
import Data.Maybe

data Result' = Tie' | Win' deriving (Show)

data GameStatus' = GameInProgress' | GameOver' Result' deriving (Show)

data CastlingEligiblity = NoneEligible | QueenSideEligible | KingSideEligible | BothEligible deriving (Show, Eq)

data GameState = GameState
  { board :: Board
  , turn :: Player
  , playerStates :: [(Player, CastlingEligiblity)]
  , lastPawnLeap :: Maybe File -- The file in which the pawn advances two squares (to be attacked by adjacent pawns)
  , status' :: GameStatus' -- GameStatus' + Player = GameStatus
  }
  deriving (Show)

data Result = Tie | Win Player deriving (Show)

data GameStatus = GameInProgress | GameOver Result deriving (Show)

status :: GameState -> GameStatus
status state = untagStatus $ status' state
 where
  untagStatus GameInProgress' = GameInProgress
  untagStatus (GameOver' r') = GameOver (untagResult r')
  untagResult Tie' = Tie
  untagResult Win' = Win (turn state)

nextTurn :: GameState -> Player
nextTurn = opponent . turn

getPlayerState :: GameState -> Player -> CastlingEligiblity
getPlayerState GameState{playerStates} player = fromJust $ lookup player playerStates

initialGameState :: GameState
initialGameState = GameState initialBoard White initialStates Nothing GameInProgress'
 where
  initialStates = [(White, BothEligible), (Black, BothEligible)]
