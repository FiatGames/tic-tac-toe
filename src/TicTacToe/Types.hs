{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module TicTacToe.Types where

import           Control.Applicative
import           Control.Lens        hiding ((|>))
import           Data.Bool
import           Data.Hashable       (Hashable, hashWithSalt)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.List           (foldl', transpose)
import           Data.Maybe
import           Data.Sequence       (Seq, (|>))
import           GHC.Generics

data Player = X | O
  deriving (Eq,Show,Generic)

data Spot = UL | UM | UR | ML | MM | MR | DL | DM | DR
  deriving (Eq,Show,Enum,Generic)
instance Hashable Spot
  where hashWithSalt i s = i + fromEnum s

data GameOver = Win Player | Draw
  deriving (Eq,Show,Generic)

data Move
  = Place Spot
  deriving (Eq,Show,Generic)

type Board = HashMap Spot (Maybe Player)

data GameState = GameState
  { _gameStateBoard    :: !Board
  , _gameStateTurn     :: !Player
  , _gameStateMoves    :: !(Seq Move)
  , _gameStateGameOver :: !(Maybe GameOver)
  } deriving (Eq,Show,Generic)
makeLenses ''GameState

emptyBoard :: Board
emptyBoard = HM.fromList (map (flip (,) Nothing) $ enumFrom UL)

initialState :: GameState
initialState = GameState emptyBoard X mempty Nothing

validMoves :: GameState -> [Move]
validMoves (GameState _ _ _ (Just _)) = []
validMoves (GameState b _ _ Nothing) = HM.foldlWithKey' (\l s -> maybe (Place s:l) (const l)) [] b

winningLines :: [[Spot]]
winningLines = upDown ++ leftRight ++ diag
  where
    upDown =
      [ [UL, UM, UR]
      , [ML, MM, MR]
      , [DL, DM, DR]
      ]
    leftRight = transpose upDown
    diag =
      [ [UL, MM, DR]
      , [UR, MM, DL]
      ]

winner :: Board -> Player -> Maybe GameOver
winner b t = boolToMaybe (Win t) won <|> boolToMaybe Draw tie
  where
    boolToMaybe f = bool Nothing (Just f)
    tie = all isJust b
    won = any (all (\s -> b ! s == Just t)) winningLines

makeMove :: GameState -> Move -> GameState
makeMove gs@(GameState _ _ _ (Just _)) _ = gs
makeMove (GameState b t ms Nothing) mv@(Place m)
  = gs'
  where
    gs' = GameState b' t' ms' (winner b' t)
    ms' = ms |> mv
    b' = HM.update (const $ Just $ Just t) m b
    t' = case t of
      X -> O
      O -> X

playGame :: [Move] -> GameState
playGame = foldl' makeMove initialState

game1 :: [Move]
game1 = [Place UL,Place MM,Place UM, Place DM,Place UR]
game2 :: [Move]
game2 = [Place UL,Place MM,Place UM,Place DM,Place MR]
