module API (products) where

import Http
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (apply)
import Schema exposing (..)
import Signal exposing (constant)

products : Signal (Http.Response (List Product))
products = Signal.map (Http.mapResult <| decodeString (at ["results"] decodeProducts))
                      (Http.sendGet <| constant "products.json")

decodeDimensions : Decoder Dimensions
decodeDimensions = Dimensions
  `map`   ("w" := int)
  `apply` ("h" := int)

decodeImage : Decoder Image
decodeImage = Image
  `map`   ("dimensions" := decodeDimensions)
  `apply` ("url" := string)

decodeVariant : Decoder Variant
decodeVariant = Variant
  `map` ("images" := list decodeImage)

decodeProduct : Decoder Product
decodeProduct = Product
  `map` ("id" := string)
  `apply` ("name" := dict string)
  `apply` ("masterVariant" := decodeVariant)

decodeProducts : Decoder (List Product)
decodeProducts = (list decodeProduct)
