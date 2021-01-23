module ServerMain where

import Prelude
import Data.Api (api)
import Data.Array (find)
import Data.Blanket (Blanket)
import Data.Material (Material(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.HTTP (createServer, listen)
import Nodetrout (serve')

blankets :: Array Blanket
blankets =
  [ { id: 1
    , name: "cotton blanket"
    , madeOf: Cotton
    }
  , { id: 2
    , name: "wool blanket"
    , madeOf: Wool
    }
  , { id: 3
    , name: "polyester blanket"
    , madeOf: Polyester
    }
  ]

findBlanket :: Int -> Maybe Blanket
findBlanket i = find (\b -> b.id == i) blankets

main :: Effect Unit
main = do
  let
    resources =
      { blanket:
          { list: { "GET": pure blankets }
          , find: \i -> { "GET": pure $ findBlanket i }
          }
      }
  server <- createServer $ serve' api resources logShow
  listen server
    { hostname: "0.0.0.0"
    , port: 5000
    , backlog: Nothing
    }
    $ log "Listening on port 5000..."
