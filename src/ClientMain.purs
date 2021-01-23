module ClientMain where

import Prelude
import Data.Api (api)
import Data.Blanket (Blanket)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Type.Trout.Client (asClients)

type State
  = { num :: Int
    , blanketList :: Array Blanket
    , blanket :: Maybe Blanket
    }

data Action
  = RequestAll
  | RequestOne
  | UpdateNum String

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { num: 1
  , blanketList: []
  , blanket: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button
        [ HE.onClick \_ -> Just RequestAll ]
        [ HH.text "Request All" ]
    , case state.blanketList of
        [] -> HH.p_ [ HH.text "no data" ]
        bs -> HH.ul_ $ bs <#> \b -> HH.li_ [ showBlanket b ]
    , HH.input
        [ HP.type_ HP.InputNumber
        , HP.value $ show state.num
        , HE.onValueChange $ Just <<< UpdateNum
        ]
    , HH.button
        [ HE.onClick \_ -> Just RequestOne ]
        [ HH.text "Request One" ]
    , case state.blanket of
        Nothing -> HH.p_ [ HH.text "no data" ]
        Just b -> showBlanket b
    ]
  where
  showBlanket b =
    HH.ul_
      [ HH.li_ [ HH.text $ show b.id ]
      , HH.li_ [ HH.text b.name ]
      , HH.li_ [ HH.text $ show b.madeOf ]
      ]

handleAction ∷ forall o m. MonadAff m => Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  RequestAll -> do
    res <- H.liftAff task.blanket.list."GET"
    H.modify_ \st -> st { blanketList = res }
  RequestOne -> do
    s <- H.get
    res <- H.liftAff $ (task.blanket.find s.num)."GET"
    H.modify_ \st -> st { blanket = res }
  UpdateNum s -> case fromString s of
    Just n -> H.modify_ \st -> st { num = n }
    Nothing -> pure unit
  where
  task = asClients api

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
