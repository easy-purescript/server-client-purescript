module Data.Constant where

import Data.Symbol (SProxy(..))

type BlanketFlagment
  = "blanket"

blanketFlagment :: SProxy BlanketFlagment
blanketFlagment = SProxy
