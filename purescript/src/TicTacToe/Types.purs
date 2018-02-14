-- File auto generated by purescript-bridge! --
module TicTacToe.Types where

import Data.Lens (Iso', Lens', Prism', lens, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(SProxy))

import Prelude
import Data.Generic (class Generic)

data Player =
    X
  | O

derive instance genericPlayer :: Generic Player
derive instance eqPlayer :: Eq Player

--------------------------------------------------------------------------------
_X :: Prism' Player Unit
_X = prism' (\_ -> X) f
  where
    f X = Just unit
    f _ = Nothing

_O :: Prism' Player Unit
_O = prism' (\_ -> O) f
  where
    f O = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
data Spot =
    UL
  | UM
  | UR
  | ML
  | MM
  | MR
  | DL
  | DM
  | DR

derive instance genericSpot :: Generic Spot
derive instance eqSpot :: Eq Spot

--------------------------------------------------------------------------------
_UL :: Prism' Spot Unit
_UL = prism' (\_ -> UL) f
  where
    f UL = Just unit
    f _ = Nothing

_UM :: Prism' Spot Unit
_UM = prism' (\_ -> UM) f
  where
    f UM = Just unit
    f _ = Nothing

_UR :: Prism' Spot Unit
_UR = prism' (\_ -> UR) f
  where
    f UR = Just unit
    f _ = Nothing

_ML :: Prism' Spot Unit
_ML = prism' (\_ -> ML) f
  where
    f ML = Just unit
    f _ = Nothing

_MM :: Prism' Spot Unit
_MM = prism' (\_ -> MM) f
  where
    f MM = Just unit
    f _ = Nothing

_MR :: Prism' Spot Unit
_MR = prism' (\_ -> MR) f
  where
    f MR = Just unit
    f _ = Nothing

_DL :: Prism' Spot Unit
_DL = prism' (\_ -> DL) f
  where
    f DL = Just unit
    f _ = Nothing

_DM :: Prism' Spot Unit
_DM = prism' (\_ -> DM) f
  where
    f DM = Just unit
    f _ = Nothing

_DR :: Prism' Spot Unit
_DR = prism' (\_ -> DR) f
  where
    f DR = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
data GameOver =
    Win Player
  | Draw

derive instance genericGameOver :: Generic GameOver
derive instance eqGameOver :: Eq GameOver

--------------------------------------------------------------------------------
_Win :: Prism' GameOver Player
_Win = prism' Win f
  where
    f (Win a) = Just $ a
    f _ = Nothing

_Draw :: Prism' GameOver Unit
_Draw = prism' (\_ -> Draw) f
  where
    f Draw = Just unit
    f _ = Nothing

--------------------------------------------------------------------------------
