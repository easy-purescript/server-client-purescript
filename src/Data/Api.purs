module Data.Api where

import Data.Blanket (Blanket)
import Data.Maybe (Maybe)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Lit, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

type BlanketFlagment
  = "blankets"

type ApiFlagment
  = "api"

type Api
  = ApiFlagment
      :/ ( "blanketList" := Lit BlanketFlagment :> Resource (Get (Array Blanket) JSON)
          :<|> "blanket"
          := BlanketFlagment
          :/ Capture "id" Int
          :> Resource (Get (Maybe Blanket) JSON)
      )

api :: Proxy Api
api = Proxy
