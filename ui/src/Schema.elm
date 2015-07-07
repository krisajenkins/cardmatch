module Schema where

import Dict exposing (Dict)

type alias ProductId = String

type alias Dimensions =
  {width : Int
  ,height : Int}

type alias Image =
  {dimensions : Dimensions
  ,url : String}

type alias Variant =
  {images : List Image}

type alias Product =
  {id : ProductId
  ,name : Dict String String
  ,masterVariant : Variant}
