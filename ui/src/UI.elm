module UI (rootView) where

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Response(..))
import Schema exposing (..)
import Set exposing (Set)
import Signal exposing (Address)
import System exposing (..)
import UICommon exposing (..)

rootView : Address Action -> Model -> Html
rootView uiChannel model =
  div [id "root-view"
      ,class "container-fluid"]
      [div [class "row"]
           [div [class "col-xs-12 col-md-10 col-md-offset-1"]
                [mainContentView uiChannel model]]]

mainContentView : Address Action -> Model -> Html
mainContentView uiChannel model =
  case (model.products, model.currentGame) of
    (NotAsked,_) -> loadingView
    (Waiting,_) -> loadingView
    (Failure code message,_) -> errorView message
    (Success ps,Nothing) -> startGameView uiChannel
    (Success ps,Just game) -> if isComplete game
                              then winnerView uiChannel
                              else gameView uiChannel ps game

------------------------------------------------------------

startGameView : Address Action -> Html
startGameView uiChannel =
  div [class "row"]
      [div [class "col-xs-12 col-sm-4 col-sm-offset-4"]
           [h1 [] [text "Cape Match"]
           ,button [class "btn btn-primary btn-block btn-lg"
                   ,onClick uiChannel NewGame]
                   [text "Start New Game"]]]

------------------------------------------------------------

gameView : Address Action -> Dict ProductId Product -> Game -> Html
gameView uiChannel products game =
  div []
      [div [class "row"]
           (List.map (cardView uiChannel products game.guesses)
                     game.cards)]

cardView : Address Action -> Dict ProductId Product -> Set Guess -> Card -> Html
cardView uiChannel products guesses card =
  let (cardId,_) = card
  in div [class "col-xs-4 col-sm-2"]
         (case Dict.get (snd cardId) products of
            Nothing -> [errorView "Product Not Found"]
            Just p -> [div [classList [("card", True)
                                      ,("active", isActive guesses card)]
                           ,onClick uiChannel (NewGuess cardId)]
                           [div [class "back"]
                                []
                           ,div [class "face"]
                                [productImage p]]])

productImage : Product -> Html
productImage p =
  case (List.head p.masterVariant.images) of
    Nothing -> loadingView
    Just i -> div [class "product-image"
                  ,style [("backgroundImage", "url(" ++ i.url ++ ")")]
                  ,width i.dimensions.width
                  ,height i.dimensions.height]
                  []

------------------------------------------------------------

winnerView : Address Action -> Html
winnerView uiChannel =
  div [class "row"]
      [div [class "col-xs-12 col-sm-4 col-sm-offset-4"]
           [h1 [] [text "You Win!"]
           ,p [] [text "Congratulations! Use this code to receive 10% off your next purchase!"]
           ,h4 [class "offer-code"] [text "Code: CARDFLIPPER"]
           ,button [class "btn btn-primary btn-block btn-lg"
                   ,onClick uiChannel NewGame]
                   [text "Play Again"]]]
