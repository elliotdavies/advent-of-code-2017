module Lib where

import Input (input)

sumMatchingConsecutives :: [Integer] -> Integer
sumMatchingConsecutives [] = 0
sumMatchingConsecutives xs =
  let
    first =
      head xs

    sumHelper ys =
      case ys of
        (x:[]) ->
          if first == x then first else 0

        (x:y:zs) ->
          let
            rest = sumHelper (y:zs)
          in
            if x == y then x + rest else rest
  in
    sumHelper xs

solution :: Integer
solution = sumMatchingConsecutives input
