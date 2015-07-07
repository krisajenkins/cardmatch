module System where

import Http exposing (Response(NotAsked))
import Time exposing (Time)
import Schema exposing (ProductId,Product)
import Set exposing (Set)
import Dict exposing (Dict)
import Random exposing (Seed)
import Config exposing (..)
import List exposing (length)

type alias Guess = (Time,CardId)
type alias CardId = (Int,ProductId)
type alias Showing = Bool
type alias Card = (CardId,Showing)

type alias Game =
  {cards : List Card
  ,duration : Time
  ,started : Time
  ,guesses : Set Guess}

type alias Model =
  {products : Response (Dict ProductId Product)
  ,randomSeed : Seed
  ,currentTime : Time
  ,currentGame : Maybe Game
  ,points : Int}

type Action = NoOp
            | Tick Time
            | LoadProducts (Response (List Product))
            | NewGame
            | NewGuess CardId

------------------------------------------------------------

fmapGame : (Game -> Game) -> Model -> Model
fmapGame f model = { model | currentGame <- Maybe.map f model.currentGame }

isComplete : Game -> Bool
isComplete game =
  let showings = List.map snd game.cards
  in List.foldl (&&) True showings

isActive : Set Guess -> Card -> Bool
isActive guesses card =
  let (cardId,showing) = card
  in showing || Set.member cardId (Set.map snd guesses)

isOld : Time -> Guess -> Bool
isOld expires guess =
  let (time,_) = guess
  in expires < time + guessDuration

-- | A guess is correct if there are two guesses and they all have the same product ID.
isCorrect : Set Guess -> Bool
isCorrect guesses =
  length (Set.toList guesses) == 2
  &&
  length (Set.toList (Set.map (snd << snd) guesses)) == 1

expireGuesses : Time -> Game -> Game
expireGuesses t game =
  let expiredGuesses = Set.filter (isOld t) game.guesses
  in {game | guesses <- if expiredGuesses == Set.empty
                        then Set.empty
                        else game.guesses}

markShowing : Set CardId -> Card -> Card
markShowing cardIds card =
  if Set.member (fst card) cardIds
  then (fst card, True)
  else card

recordGuess : Guess -> Game -> Game
recordGuess guess game =
  let newGuesses = Set.insert guess game.guesses
  in if | length (Set.toList newGuesses) > 2 -> game
        | isCorrect newGuesses -> { game | guesses <- Set.empty
                                         , cards <- List.map (markShowing (Set.map snd newGuesses)) game.cards}
        | otherwise -> { game | guesses <- newGuesses }

------------------------------------------------------------

initialModel : Time -> Seed -> Model
initialModel t seed =
  {products = NotAsked
  ,randomSeed = seed
  ,currentTime = t
  ,points = 0
  ,currentGame = Nothing}
