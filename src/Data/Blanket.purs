module Data.Blanket where

import Data.Material (Material)

type Blanket
  = { id :: Int
    , name :: String
    , madeOf :: Material
    }
