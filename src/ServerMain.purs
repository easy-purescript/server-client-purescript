module ServerMain where

import Prelude
import Control.Monad.Except (ExceptT)
import Data.Array (find)
import Data.Blanket (Blanket)
import Data.Constant (BlanketFlagment)
import Data.Material (Material(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout (HTTPError, serve')
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, Lit, Resource)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)

type AppM a
  = ExceptT HTTPError Aff a

type Site
  = "blanketList" := Lit BlanketFlagment :> Resource (Get (Array Blanket) JSON)
      :<|> "blanket"
      := BlanketFlagment
      :/ Capture "id" Int
      :> Resource (Get (Maybe Blanket) JSON)

site :: Proxy Site
site = Proxy

blankets :: Array Blanket
blankets =
  [ { id: 1, name: "cotton blanket", madeOf: Cotton }
  , { id: 2, name: "wool blanket", madeOf: Wool }
  , { id: 3, name: "polyester blanket", madeOf: Polyester }
  ]

findBlanket :: Int -> Maybe Blanket
findBlanket i = find (eq i <<< _.id) blankets

resources ::
  { blanketList :: { "GET" :: AppM (Array Blanket) }
  , blanket :: Int -> { "GET" :: AppM (Maybe Blanket) }
  }
resources =
  { blanketList: { "GET": pure blankets }
  , blanket: \i -> { "GET": pure $ findBlanket i }
  }

main :: Effect Unit
main = do
  server <- createServer $ serve' site resources (const $ pure unit)
  listen server { hostname: "0.0.0.0", port: 3000, backlog: Nothing } $ log "Listening on port 3000..."
