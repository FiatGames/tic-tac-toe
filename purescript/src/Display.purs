module Display where

import Prelude

import Data.Array (findIndex, index)
import Data.Generic (gCompare, gEq, gShow)
import Data.Tuple (Tuple(..), snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (input)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Spot as Spot
import TicTacToe.DTO (GameState(..))
import TicTacToe.Types (Spot(..))

type Input = GameState

type State = GameState

data Query a 
  = HandleInput GameState a
  | HandleSpot Spot Spot.Message a

data Message = SpotChosen Spot

newtype Slot = Slot Spot
instance showSlot :: Show Slot where
  show (Slot s1) = gShow s1
instance eqSlot :: Eq Slot where
  eq (Slot s1) (Slot s2) = gEq s1 s2
instance ordSlot :: Ord Slot where
  compare (Slot s1) (Slot s2) = gCompare s1 s2

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where
  
  render :: State -> H.ParentHTML Query Spot.Query Slot m
  render (GameState s) = HH.div 
    [HP.classes [H.ClassName "container-fluid"]]
    [ HH.div [HP.classes [H.ClassName "row"]]
      [ spot UL, spot UM, spot UR ]
    , HH.div [HP.classes [H.ClassName "row"]]
      [ spot ML, spot MM, spot MR]
    , HH.div [HP.classes [H.ClassName "row"]]
      [ spot DL, spot DM, spot DR]
    ]
    where
      spot sp = HH.slot (Slot sp) Spot.component (getSpot sp) (HE.input $ HandleSpot sp)
      getSpot sp = join $ map snd $ findIndex (\(Tuple s _) -> (Slot s) == (Slot sp)) s.board >>= index s.board

  eval :: Query ~> H.ParentDSL State Query Spot.Query Slot Message m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      H.put n
      pure next
    HandleSpot sp m next -> case m of
      Spot.Chosen -> do
        H.raise $ SpotChosen sp
        pure next