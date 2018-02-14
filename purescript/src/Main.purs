module Main where

import Prelude
import TicTacToe.DTO
import TicTacToe.Types

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either)

gameOverStr :: String
gameOverStr = "{\"gameStateBoard\":[[\"UL\",\"X\"],[\"UM\",\"X\"],[\"UR\",\"X\"],[\"ML\",null],[\"MM\",\"O\"],[\"MR\",null],[\"DL\",null],[\"DM\",\"O\"],[\"DR\",null]],\"gameStateTurn\":\"O\",\"gameStateMoves\":[\"UL\",\"MM\",\"UM\",\"DM\",\"UR\"],\"gameStateGameOver\":{\"tag\":\"Win\",\"contents\":\"X\"},\"gameStateValidMoves\":[]}"
inProgressStr :: String
inProgressStr = "{\"gameStateBoard\":[[\"UL\",\"X\"],[\"UM\",\"X\"],[\"UR\",null],[\"ML\",null],[\"MM\",\"O\"],[\"MR\",\"X\"],[\"DL\",null],[\"DM\",\"O\"],[\"DR\",null]],\"gameStateTurn\":\"O\",\"gameStateMoves\":[\"UL\",\"MM\",\"UM\",\"DM\",\"MR\"],\"gameStateGameOver\":null,\"gameStateValidMoves\":[\"DR\",\"DL\",\"ML\",\"UR\"]}"

gameOver :: Either String String
gameOver = map (const "Valid") $ (jsonParser gameOverStr >>= decodeJson) :: Either String GameState

inProgress :: Either String String
inProgress = map (const "Valid") $ (jsonParser inProgressStr >>= decodeJson) :: Either String GameState

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"