module Spot where

import Prelude
import TicTacToe.DTO

import Data.Maybe (Maybe(..), maybe)
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TicTacToe.Types (Player(..))

type Input = Maybe Player

type State = Maybe Player

data Query a 
  = HandleInput State a
  | HandleChose a

data Message = Chosen

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render mP = HH.div 
      [ HP.classes [ClassName "col-xs-4"]
      , HP.attr (AttrName "style") "text-align:center;cursor:pointer;"
      , HE.onClick (HE.input_ HandleChose)
      ]
      [HH.text $ maybe "-" showPlayer mP]
  
  showPlayer X = "X"
  showPlayer O = "O"

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      H.put n
      pure next
    HandleChose next -> do
      H.raise Chosen
      pure next
