module Display where

import Prelude
import TicTacToe.DTO

import Data.Array (findIndex, index)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (attr)
import Halogen.HTML.Properties as HP
import TicTacToe.Types (Player(..), Spot(..))

type Input = GameState

type State = GameState

data Query a = HandleInput GameState a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render (GameState s) = HH.div [HP.classes [ClassName "container-fluid"]]
    [ HH.div [HP.classes [ClassName "row"]]
      [ HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == UL) s.board >>= index s.board))]
      , HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == UM) s.board >>= index s.board))]
      , HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == UR) s.board >>= index s.board))]
      ]
    , HH.div [HP.classes [ClassName "row"]]
      [ HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == ML) s.board >>= index s.board))]
      , HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == MM) s.board >>= index s.board))]
      , HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == MR) s.board >>= index s.board))]
      ]
    , HH.div [HP.classes [ClassName "row"]]
      [ HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == DL) s.board >>= index s.board))]
      , HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == DM) s.board >>= index s.board))]
      , HH.div [HP.classes [ClassName "col-sm-4"], HP.attr (AttrName "style") "text-align:center;"]
        [HH.text (maybe "" pl (findIndex (\(Tuple s _) -> s == DR) s.board >>= index s.board))]
      ]
    ]

  pl (Tuple s Nothing) = "-"
  pl (Tuple s (Just O)) = "O"
  pl (Tuple s (Just X)) = "X"
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      H.put n
      pure next