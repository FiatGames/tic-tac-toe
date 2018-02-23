{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module TicTacToe.Types where

import           Control.Applicative
import           Control.Lens        hiding ((|>))
import           Control.Monad
import           Data.Bool
import           Data.Hashable       (Hashable, hashWithSalt)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.List           (transpose)
import           Data.Maybe
import           Data.Sequence       (Seq, (|>))
import           Data.Text           (Text)
import           FiatGame            (FiatMove (..), FiatPlayer (..))
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
  = SetPlayer Player
  | Place Spot
  deriving (Eq,Show,Generic)

type Board = HashMap Spot (Maybe Player)

data GameState = GameState
  { _gameStateBoard    :: !Board
  , _gameStateTurn     :: !Player
  , _gameStateMoves    :: !(Seq (FiatMove Move))
  , _gameStateGameOver :: !(Maybe GameOver)
  , _gameStateXPlayer  :: Maybe FiatPlayer
  , _gameStateOPlayer  :: Maybe FiatPlayer
  } deriving (Eq,Show,Generic)
makeLenses ''GameState

emptyBoard :: Board
emptyBoard = HM.fromList (map (flip (,) Nothing) $ enumFrom UL)

initialState :: GameState
initialState = GameState emptyBoard X mempty Nothing Nothing Nothing

validMoves :: GameState -> [FiatMove Move]
validMoves (GameState b p _ Nothing (Just x) (Just o)) = HM.foldlWithKey' (\l s -> maybe (FiatMove pl (Place s):l) (const l)) [] b
  where pl = case p of
              X -> x
              O -> o
validMoves (GameState _ _ _ Nothing _ _) = []
validMoves (GameState _ _ _ (Just _) _ _) = []

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

makeMove :: GameState -> FiatMove Move -> Either Text GameState
makeMove gs (FiatMove (FiatPlayer p) (SetPlayer X)) = Right $ gs & gameStateXPlayer .~ Just (FiatPlayer p)
makeMove gs (FiatMove (FiatPlayer p) (SetPlayer O)) = Right $ gs & gameStateOPlayer .~ Just (FiatPlayer p)
makeMove (GameState _ _ _ _ Nothing _) (FiatMove _ (Place _)) = Left "Players not set up"
makeMove (GameState _ _ _ _ _ Nothing) (FiatMove _ (Place _)) = Left "Players not set up"
makeMove (GameState _ _ _ (Just _) _ _) _ = Left "Game is over"
makeMove (GameState b t ms Nothing (Just (FiatPlayer x)) (Just (FiatPlayer y))) mv@(FiatMove (FiatPlayer p) (Place m))
  | p /= x && p /= y = Left "Player not in game"
  | p == x && t /= X = Left "It is not the players turn"
  | p == y && t /= O = Left "It is not the players turn"
  | otherwise = Right gs'
  where
    gs' = GameState b' t' ms' (winner b' t) (Just (FiatPlayer x)) (Just (FiatPlayer y))
    ms' = ms |> mv
    b' = HM.update (const $ Just $ Just t) m b
    t' = case t of
      X -> O
      O -> X

playGame :: [FiatMove Move] -> Either Text GameState
playGame = foldM makeMove initialState

game1 :: [FiatMove Move]
game1 = [FiatMove (FiatPlayer 0) (SetPlayer X), FiatMove (FiatPlayer 1) (SetPlayer O), FiatMove (FiatPlayer 0) (Place UL),FiatMove (FiatPlayer 1) (Place MM),FiatMove (FiatPlayer 0) (Place UM),FiatMove (FiatPlayer 1) (Place DM),FiatMove (FiatPlayer 0) (Place UR)]
game2 :: [FiatMove Move]
game2 = [FiatMove (FiatPlayer 0) (SetPlayer X), FiatMove (FiatPlayer 1) (SetPlayer O), FiatMove (FiatPlayer 0) (Place UL),FiatMove (FiatPlayer 1) (Place MM),FiatMove (FiatPlayer 0) (Place UM),FiatMove (FiatPlayer 1) (Place DM),FiatMove (FiatPlayer 0) (Place MR)]
