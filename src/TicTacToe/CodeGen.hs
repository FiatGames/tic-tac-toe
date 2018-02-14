{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module TicTacToe.CodeGen where

import           Data.Proxy
import           Language.PureScript.Bridge
import qualified TicTacToe.DTO              as DTO
import qualified TicTacToe.Types            as T

bridge :: BridgePart
bridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy T.Player)
  , mkSumType (Proxy :: Proxy T.Spot)
  , mkSumType (Proxy :: Proxy T.GameOver)
  , mkSumType (Proxy :: Proxy DTO.GameState)
  ]

main :: IO ()
main = writePSTypes "purescript/src" (buildBridge bridge) myTypes
