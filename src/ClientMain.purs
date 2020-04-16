module ClientMain where

import Prelude
import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Bifunctor (lmap)
import Data.Blanket (Blanket)
import Data.Constant (blanketFlagment)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State
  = { num :: Int
    , blankets :: Either String (Array Blanket)
    , blanket :: Either String (Maybe Blanket)
    }

data Action
  = RequestAll
  | RequestOne
  | UpdateNum String

baseUrl :: String
baseUrl = "http://localhost:3000"

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
  , blankets: Right []
  , blanket: Right Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button
        [ HE.onClick \_ -> Just RequestAll ]
        [ HH.text "Request" ]
    , HH.p_
        [ HH.text $ show state.blankets ]
    , HH.input
        [ HP.type_ HP.InputNumber
        , HP.value $ show state.num
        , HE.onValueChange $ Just <<< UpdateNum
        ]
    , HH.button
        [ HE.onClick \_ -> Just RequestOne ]
        [ HH.text "Request One" ]
    , HH.p_
        [ HH.text $ show state.blanket ]
    ]

handleAction ∷ forall o m. MonadAff m => Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  RequestAll -> do
    res <- H.liftAff $ AX.get AXRF.json (baseUrl <> "/" <> reflectSymbol blanketFlagment)
    H.modify_ \st -> st { blankets = decodeResponse res }
  RequestOne -> do
    s <- H.get
    res <- H.liftAff $ AX.get AXRF.json (baseUrl <> "/" <> reflectSymbol blanketFlagment <> "/" <> show s.num)
    H.modify_ \st -> st { blanket = decodeResponse res }
  UpdateNum s -> case fromString s of
    Just n -> H.modify_ \st -> st { num = n }
    Nothing -> pure unit

decodeResponse :: forall a. DecodeJson a => Either AX.Error (AX.Response Json) -> Either String a
decodeResponse = decodeJson <=< map _.body <<< lmap printError

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
