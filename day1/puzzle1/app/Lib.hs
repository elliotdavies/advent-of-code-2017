module Lib where

import Input (input)

-- Puzzle 1
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

solution1 :: Integer
solution1 = sumMatchingConsecutives input

-- Puzzle 2
sumHalfwayMatches :: [Integer] -> Integer
sumHalfwayMatches [] = 0
sumHalfwayMatches xs = 0

solution2 :: Integer
solution2 = sumHalfwayMatches input
