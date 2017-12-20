module Lib where

import           Data.List (elemIndices, scanl')

data Buffer = Buffer Int [Int] deriving (Show)

runUntil :: Int -> Int -> Buffer
runUntil n spin =
  step 1 $ Buffer 0 [0]
  where
    step :: Int -> Buffer -> Buffer
    step val (Buffer pos buff) =
      let
        pos' = pos + spin + 1 `mod` length buff
        (xs,ys) = splitAt pos' buff
        buffer' = Buffer pos' (xs ++ [val] ++ ys)

      in if val == n then buffer' else step (val + 1) buffer'

solution1 :: Int
solution1 =
  let (Buffer pos buff) = runUntil 2017 337
  in buff !! (pos + 1 `mod` length buff)

-- Instead of the whole Buffer, only generate a list of input positions
positions :: Int -> [Int]
positions spin = scanl' nextPos 0 [1..]
  where
    nextPos pos size = (pos + spin) `rem` size + 1

-- We want the element after 0 i.e. 1
elemAfterZero :: Int -> Int
elemAfterZero = last . elemIndices 1 . take 50000000 . positions

solution2 :: Int
solution2 = elemAfterZero 337
