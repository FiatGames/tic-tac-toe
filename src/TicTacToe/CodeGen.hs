{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TicTacToe.CodeGen where

import Language.PureScript.Bridge
import TicTacToe.Types
import Data.Proxy
  
bridge :: BridgePart
bridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy Player)
  , mkSumType (Proxy :: Proxy Spot)
  , mkSumType (Proxy :: Proxy GameOver)
  , mkSumType (Proxy :: Proxy GameStateDTO)
  ]

main :: IO ()
main = writePSTypes "purescript/src" (buildBridge bridge) myTypes