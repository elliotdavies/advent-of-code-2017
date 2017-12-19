module Lib where

import Data.List (inits)
import Text.Printf (printf)

factorA = 16807
factorB = 48271

next :: Int -> Int -> Int
next factor = (`mod` 2147483647) . (* factor)

pairs :: (Int,Int) -> [(Int,Int)]
pairs (seedA,seedB) =
  zip (iterate (next factorA) seedA)
      (iterate (next factorB) seedB)

choosyPairs :: (Int,Int) -> [(Int,Int)]
choosyPairs (seedA,seedB) =
  zip (filter (`divBy` 4) $ iterate (next factorA) seedA)
      (filter (`divBy` 8) $ iterate (next factorB) seedB)
  where
    x `divBy` y = x `mod` y == 0

matches :: Int -> [(Int,Int)] -> Int
matches n = length . filter (uncurry (==) . mapPair (last16 . toBinary)) . take n
  where
    toBinary :: Int -> String
    toBinary n = printf "%0b" n

    last16 :: String -> String
    last16 = reverse . take 16 . reverse

    mapPair :: (a -> b) -> (a, a) -> (b, b)
    mapPair f (a,b) = (f a, f b)

-- Part 1
test1 :: Int
test1 = matches 5 $ pairs (65,8921)

solution1 :: Int
solution1 = matches 40000000 $ pairs (116,299)

-- Part 2
test2 :: Int
test2 = matches 5000000 $ choosyPairs (65,8921)

solution2 :: Int
solution2 = matches 5000000 $ choosyPairs (116,299)
