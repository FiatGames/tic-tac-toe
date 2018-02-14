module Main where

import Prelude
import TicTacToe.DTO
import TicTacToe.Types

import Control.Monad.Eff (Eff)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either, fromRight)
import Display as D
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)

gameOverStr :: String
gameOverStr = "{\"board\":[[\"UL\",\"X\"],[\"UM\",\"X\"],[\"UR\",\"X\"],[\"ML\",null],[\"MM\",\"O\"],[\"MR\",null],[\"DL\",null],[\"DM\",\"O\"],[\"DR\",null]],\"turn\":\"O\",\"moves\":[\"UL\",\"MM\",\"UM\",\"DM\",\"UR\"],\"gameOver\":{\"tag\":\"Win\",\"contents\":\"X\"},\"validMoves\":[]}"
inProgressStr :: String
inProgressStr = "{\"board\":[[\"UL\",\"X\"],[\"UM\",\"X\"],[\"UR\",null],[\"ML\",null],[\"MM\",\"O\"],[\"MR\",\"X\"],[\"DL\",null],[\"DM\",\"O\"],[\"DR\",null]],\"turn\":\"O\",\"moves\":[\"UL\",\"MM\",\"UM\",\"DM\",\"MR\"],\"gameOver\":null,\"validMoves\":[\"DR\",\"DL\",\"ML\",\"UR\"]}"

gameOver :: Either String GameState
gameOver = jsonParser gameOverStr >>= decodeJson

inProgress :: Either String GameState
inProgress = jsonParser inProgressStr >>= decodeJson

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI D.component (unsafePartial $ fromRight gameOver) body