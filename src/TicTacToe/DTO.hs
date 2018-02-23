{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TicTacToe.DTO where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import           FiatGame
import           GHC.Exts             (fromList, toList)
import           GHC.Generics         hiding (from)
import qualified TicTacToe.Types      as T

$(deriveJSON defaultOptions ''T.Spot)
$(deriveJSON defaultOptions ''T.Player)
$(deriveJSON defaultOptions ''T.GameOver)
$(deriveJSON defaultOptions ''T.Move)

data GameState =  GameState
  { board      :: ![(T.Spot, Maybe T.Player)]
  , turn       :: !T.Player
  , moves      :: ![FiatMove T.Move]
  , gameOver   :: !(Maybe T.GameOver)
  , validMoves :: ![FiatMove T.Move]
  , xPlayer    :: Maybe FiatPlayer
  , oPlayer    :: Maybe FiatPlayer
  } deriving (Eq,Show,Generic)
$(deriveJSON defaultOptions ''GameState)

toGameStateDTO :: T.GameState -> GameState
toGameStateDTO gs@(T.GameState b t mvs o xP oP) = GameState (toList b) t (toList mvs) o (T.validMoves gs) xP oP
fromGameStateDTO :: GameState -> T.GameState
fromGameStateDTO (GameState b t mvs o _ xP oP) = T.GameState (fromList b) t (fromList mvs) o xP oP

instance FiatGame T.GameState GameState T.Move where
  initialState = T.initialState
  makeMove :: T.GameState -> FiatMove T.Move -> Either Text T.GameState
  makeMove = T.makeMove
  playerAllowed :: T.GameState -> FiatPlayer -> FiatMove T.Move -> Bool
  playerAllowed _ p1 (FiatMove p2 (T.SetPlayer _)) = p1 == p2
  playerAllowed _ p1 (FiatMove p2 (T.Place _))     = p1 == p2
  gameStateIso = iso toGameStateDTO fromGameStateDTO

data NoGame = NoGame
$(deriveJSON defaultOptions ''NoGame)
data NoMove = NoMove
$(deriveJSON defaultOptions ''NoMove)
instance FiatGame NoGame NoGame NoMove where
  initialState = NoGame
  makeMove _ _ = Right NoGame
  playerAllowed _ _ _ = True
  gameStateIso = iso id id

game1BS = map encode T.game1
notAGame = map encode [FiatMove 0 NoMove, FiatMove 0 NoMove]

data Game = TicTacToe | Nope
  deriving (Eq,Show,Generic)

foo :: Game -> [ByteString] -> ByteString
foo TicTacToe mvs = encode (view gameStateIso <$> (playGame mvs :: Either Text T.GameState))
foo Nope mvs      = encode (view gameStateIso <$> (playGame mvs :: Either Text NoGame))
