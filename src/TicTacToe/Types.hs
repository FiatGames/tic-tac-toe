{-# LANGUAGE DeriveGeneric              #-}

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
import GHC.Generics
import           Data.Aeson.Types      (FromJSON, ToJSON (..), defaultOptions, genericToEncoding)
import GHC.Exts (toList)

data Player = X | O
  deriving (Eq,Show, Generic)
instance FromJSON Player
instance ToJSON Player where
  toEncoding = genericToEncoding defaultOptions
  
data GameState = GameState 
  { gameStateBoard :: !Board
  , gameStateTurn :: !Player
  , gameStateMoves :: !(Seq Move)
  } deriving (Eq,Show, Generic)

data GameStateDTO =  GameStateDTO
  { gameStateDTOBoard :: ![(Spot, Maybe Player)]
  , gameStateDTOTurn :: !Player
  , gameStateDTOMoves :: ![Move]
  } deriving (Eq,Show, Generic)
instance FromJSON GameStateDTO
instance ToJSON GameStateDTO where
  toEncoding = genericToEncoding defaultOptions

data Spot = UL | UM | UR | ML | MM | MR | DL | DM | DR
  deriving (Eq,Show,Enum, Generic)
instance Hashable Spot
  where hashWithSalt i s = i + fromEnum s
instance FromJSON Spot
instance ToJSON Spot where
  toEncoding = genericToEncoding defaultOptions

data GameOver = Win Player | Draw
  deriving (Eq,Show, Generic)
instance FromJSON GameOver
instance ToJSON GameOver where
  toEncoding = genericToEncoding defaultOptions

type Move = Spot
type Board = HashMap Spot (Maybe Player)

toGameStateDTO :: GameState -> GameStateDTO
toGameStateDTO (GameState b t mvs) = GameStateDTO (toList b) t (toList mvs)
emptyBoard :: Board
emptyBoard = HM.fromList (map (flip (,) Nothing) $ enumFrom UL)

initialState :: GameState
initialState = GameState emptyBoard X mempty

possibleMoves :: GameState -> [Move]
possibleMoves (GameState b _ _) = HM.foldlWithKey' (\l s mp -> maybe (s:l) (const l) mp) [] b

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