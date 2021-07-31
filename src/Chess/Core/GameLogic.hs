module Chess.Core.GameLogic where

import Chess.Core.Models
import Chess.Core.MoveConstraints.Legal
import Utils

tryMakeMove :: GameState -> Move -> Maybe GameState
tryMakeMove gameState move@(Move source dest) =
  let isLegal = not . null $ legalMC gameState source [dest]
   in boolToMaybe isLegal $ makeMove gameState move

makeMove :: GameState -> Move -> GameState
makeMove = undefined
