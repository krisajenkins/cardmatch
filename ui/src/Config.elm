module Config where

import Time exposing (Time,second,millisecond,minute,every)

gameDuration : Float
gameDuration = 3 * minute

guessDuration : Time
guessDuration = 2 * second
