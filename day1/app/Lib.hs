module Lib where

import Input (input)

-- Puzzle 1
sumMatchingConsecutives :: [Integer] -> Integer
sumMatchingConsecutives [] = 0
sumMatchingConsecutives (x:xs) =
  sumHelper (x:xs)
  where
    comp a b = if a == b then a else 0

    sumHelper (y:[]) = comp x y
    sumHelper (y:z:ys') = (comp y z) + sumHelper (z:ys')

solution1 :: Integer
solution1 = sumMatchingConsecutives input

-- Puzzle 2
sumHalfwayMatches :: [Integer] -> Integer
sumHalfwayMatches [] = 0
sumHalfwayMatches xs =
  let
    halfLen =
      (length xs) `div` 2

    findFrom i =
      xs !! ((i + halfLen) `mod` length xs)

    sumHelper (i,x) acc =
      if x == findFrom i then acc + x else acc

  in
    foldr sumHelper 0 $ zip [0..] xs

solution2 :: Integer
solution2 = sumHalfwayMatches input
