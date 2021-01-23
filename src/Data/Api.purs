module Data.Api where

import Data.Blanket (Blanket)
import Data.Maybe (Maybe)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

type Api
  = "api"
      :/ ( "blanket" := "blanket"
          :/ ( "list" := Resource (Get (Array Blanket) JSON)
              :<|> ("find" := Capture "id" Int :> Resource (Get (Maybe Blanket) JSON))
          )
      )

api :: Proxy Api
api = Proxy
