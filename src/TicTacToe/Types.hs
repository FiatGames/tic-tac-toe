module TicTacToe.Types where

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (hashWithSalt,Hashable)
import Data.List (transpose)
import Data.Bool
import Data.Maybe
import Control.Monad
import Data.Sequence (Seq, (|>))
import Control.Applicative

data Player = X | O
  deriving (Eq,Show)

data GameState = GameState 
  { gameStateBoard :: !Board
  , gameStateTurn :: !Player
  , gameStateMoves :: !(Seq Move)
  } deriving (Eq,Show)

data Spot = UL | UM | UR | ML | MM | MR | DL | DM | DR
  deriving (Eq,Show,Enum)
instance Hashable Spot
  where hashWithSalt i s = i + fromEnum s

data GameOver = Win Player | Draw
  deriving (Eq,Show)

type Move = Spot
type Board = HashMap Spot (Maybe Player)

emptyBoard :: Board
emptyBoard = HM.fromList (map (flip (,) Nothing) $ enumFrom UL)

initialState :: GameState
initialState = GameState emptyBoard X mempty

possibleMoves :: GameState -> [Move]
possibleMoves (GameState b _ _) = HM.foldlWithKey' (\l s mp -> maybe (s:l) (const l) mp) [] b

winningLines :: [[Spot]]
winningLines = upDown ++ transpose upDown ++ diag
  where 
    upDown =
      [ [UL, UM, UR]
      , [ML, MM, MR]
      , [DL, DM, DR]
      ]
    diag = 
      [ [UL, MM, DR]
      , [UR, MM, DL]
      ]

winner :: GameState -> Maybe GameOver
winner (GameState b t _) = boolToMaybe (Win otherT) won <|> boolToMaybe Draw tie
  where
    boolToMaybe f = bool Nothing (Just f)
    tie = all isJust b
    won = any (all (\s -> b ! s == Just otherT)) winningLines
    otherT = case t of 
      X -> O
      O -> X

makeMove :: GameState -> Move -> Either (GameState, GameOver) GameState
makeMove (GameState b t ms) m = maybe (Right gs) (Left . (,) gs) (winner gs)
  where
    gs = GameState b' t' ms'
    ms' = ms |> m
    b' = HM.update (const $ Just $ Just t) m b
    t' = case t of 
      X -> O
      O -> X

playGame :: [Move] -> Either (GameState, GameOver) GameState
playGame = foldM makeMove initialState

game1 :: [Move]
game1 = [UL,MM,UM,DM,UR]