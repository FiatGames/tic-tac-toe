{-# LANGUAGE DeriveGeneric #-}
module TicTacToe.Types where

import           Control.Applicative
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

type Move = Spot
type Board = HashMap Spot (Maybe Player)

data GameState = GameState
  { gameStateBoard    :: !Board
  , gameStateTurn     :: !Player
  , gameStateMoves    :: !(Seq Move)
  , gameStateGameOver :: !(Maybe GameOver)
  } deriving (Eq,Show,Generic)

emptyBoard :: Board
emptyBoard = HM.fromList (map (flip (,) Nothing) $ enumFrom UL)

initialState :: GameState
initialState = GameState emptyBoard X mempty Nothing

validMoves :: GameState -> [Move]
validMoves (GameState b _ _ Nothing) = HM.foldlWithKey' (\l s -> maybe (s:l) (const l)) [] b
validMoves (GameState _ _ _ (Just _)) = []

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
makeMove (GameState b t ms Nothing) m = gs'
  where
    gs' = GameState b' t' ms' (winner b' t)
    ms' = ms |> m
    b' = HM.update (const $ Just $ Just t) m b
    t' = case t of
      X -> O
      O -> X

playGame :: [Move] -> GameState
playGame = foldl' makeMove initialState

game1 :: [Move]
game1 = [UL,MM,UM,DM,UR]
game2 :: [Move]
game2 = [UL,MM,UM,DM,MR]
