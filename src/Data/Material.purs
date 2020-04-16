module Data.Material where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Material
  = Cotton
  | Wool
  | Polyester

derive instance eqMaterial :: Eq Material

derive instance ordMaterial :: Ord Material

derive instance genericMaterial :: Generic Material _

instance showMaterial :: Show Material where
  show = genericShow

instance encodeJsonMaterial :: EncodeJson Material where
  encodeJson = genericEncodeJson

instance decodeJsonMaterial :: DecodeJson Material where
  decodeJson = genericDecodeJson
