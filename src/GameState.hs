module GameState (
  Result,
  GameStatus,
  CastlingEligiblity,
  GameState,
  board,
  turn,
  playerStates,
  lastEnPassant,
  status,
  initialGameState,
) where

import Board (Board, File, initialBoard)
import Player (Player (Black, White))

data Result' = Tie' | Win' deriving (Show)

data GameStatus' = GameInProgress' | GameOver' Result' deriving (Show)

data CastlingEligiblity = NoneEligible | LeftEligible | RightEligible | BothEligible deriving (Show)

data GameState = GameState
  { board :: Board
  , turn :: Player
  , playerStates :: [(Player, CastlingEligiblity)]
  , lastEnPassant :: Maybe File -- The file the en passant leaped over (to be attacked by adjacent pawns)
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

initialGameState :: GameState
initialGameState = GameState initialBoard White initialStates Nothing GameInProgress'
 where
  initialStates = [(White, BothEligible), (Black, BothEligible)]
