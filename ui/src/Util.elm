module Util where

import Random exposing (Seed)
import Array exposing (..)
import Random.Array

shuffle : Seed -> List a -> (Seed,List a)
shuffle s xs =
  let (ys,s') = Random.Array.shuffle s <| Array.fromList xs
  in (s', Array.toList ys)
