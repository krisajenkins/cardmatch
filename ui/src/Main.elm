module Main where

import Config exposing (..)
import Dict exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Http exposing (..)
import API
import List exposing (take,repeat,length,append,map2)
import Random exposing (initialSeed,Seed,Generator)
import Schema exposing (..)
import Set exposing (Set)
import Signal exposing (constant,mergeMany,foldp,(<~),Mailbox)
import System exposing (..)
import Time exposing (Time,second,millisecond,minute,every)
import UI exposing (..)
import Exts.Dict exposing (..)

port startTime : Time

currentTimeSignal : Signal Time
currentTimeSignal = every (100 * millisecond)

startSeed : Seed
startSeed = initialSeed <| round startTime

------------------------------------------------------------

uiMailbox : Mailbox Action
uiMailbox = Signal.mailbox NoOp

modelSignals : Signal Action
modelSignals = mergeMany [uiMailbox.signal
                         ,LoadProducts <~ API.products
                         ,Tick <~ currentTimeSignal]

model : Signal Model
model = foldp step (initialModel startTime startSeed) modelSignals

main : Signal Html
main = rootView uiMailbox.address <~ model

------------------------------------------------------------

step : Action -> Model -> Model
step action model =
  case action of
    NoOp -> model
    Tick t -> fmapGame (expireGuesses t) {model | currentTime <- t}
    LoadProducts response -> {model | products <- Http.map (indexBy .id) response}
    NewGuess cardId -> fmapGame (recordGuess (model.currentTime,cardId)) model
    NewGame -> case maybeNewGame model.randomSeed model.currentTime model.products of
                 Nothing -> model
                 Just (s,g) -> {model | randomSeed <- s
                                      , currentGame <- Just g}

------------------------------------------------------------

maybeNewGame : Seed -> Time -> Response (Dict ProductId Product) -> Maybe (Seed,Game)
maybeNewGame seed time products =
  case products of
    Success ps -> Just (newGame seed time ps)
    _ -> Nothing

newGame : Seed -> Time -> Dict ProductId Product -> (Seed,Game)
newGame seed time ps =
  let productIds = keys ps
      gameSize = min 6 (length productIds) -- TODO Shuffle
      lefts  = map2 (,) (repeat gameSize 0) (take gameSize productIds)
      rights = map2 (,) (repeat gameSize 1) (take gameSize productIds)
  in (seed
     ,{cards = List.map (\cardId -> (cardId,False)) (append lefts rights)
      ,duration = gameDuration
      ,started = time
      ,guesses = Set.empty})
