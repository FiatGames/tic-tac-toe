{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TicTacToe.DTO where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import           GHC.Exts             (toList)
import           GHC.Generics
import qualified TicTacToe.Types      as T

$(deriveJSON defaultOptions ''T.Spot)
$(deriveJSON defaultOptions ''T.Player)
$(deriveJSON defaultOptions ''T.GameOver)

data GameState =  GameState
  { gameStateBoard      :: ![(T.Spot, Maybe T.Player)]
  , gameStateTurn       :: !T.Player
  , gameStateMoves      :: ![T.Move]
  , gameStateGameOver   :: !(Maybe T.GameOver)
  , gameStateValidMoves :: ![T.Move]
  } deriving (Eq,Show, Generic)
$(deriveJSON defaultOptions ''GameState)

toGameStateDTO :: T.GameState -> GameState
toGameStateDTO gs@(T.GameState b t mvs go) = GameState (toList b) t (toList mvs) go (T.validMoves gs)

game1 :: GameState
game1 = toGameStateDTO $ T.playGame T.game1

testGame1 :: ByteString
testGame1 = encode game1

testGameState1 :: Maybe GameState
testGameState1 = decode testGame1
