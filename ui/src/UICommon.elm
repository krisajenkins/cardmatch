module UICommon where

import Html exposing (..)
import Html.Attributes exposing (..)

loadingView : Html
loadingView =
  div [class "loading"]
      [img [src "loading_wheel.gif"
           ,class "loading"]
           []]

errorView : String -> Html
errorView message =
  div [class "alert alert-error"]
      [h3 [] [text "Something went wrong!"]
      ,text message]
