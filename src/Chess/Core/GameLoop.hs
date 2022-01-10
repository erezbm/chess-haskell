{-# LANGUAGE TupleSections #-}

module Chess.Core.GameLoop where

import Chess.Core.GameLogic (tryMakeMove)
import Chess.Core.Models
import Chess.Core.MoveConstraint (IllegalMoveError)

class UI u where
  sendGameStarted :: u -> Player -> GameState -> IO ()
  requestMove :: u -> Player -> IO (Maybe Move)
  sendGameState :: u -> Player -> GameState -> IO ()
  sendRequestMoveFailedError :: u -> Player -> IO ()
  sendIllegalMoveError :: u -> Player -> IllegalMoveError -> IO ()

gameLoop :: (UI a) => a -> IO ()
gameLoop ui = do
  sendGameStarted ui White initialGameState
  sendGameStarted ui Black initialGameState
  loop initialGameState
 where
  loop gameState = do
    let currentPlayer = turn gameState
    mbMove <- requestMove ui currentPlayer
    case mbMove of
      Nothing -> sendRequestMoveFailedError ui currentPlayer >> loop gameState
      Just move ->
        case tryMakeMove gameState move of
          Right nextGameState -> do
            sendGameState ui currentPlayer nextGameState
            sendGameState ui (opponent currentPlayer) nextGameState
            loop nextGameState
          Left e -> do
            sendIllegalMoveError ui currentPlayer e
            loop gameState
