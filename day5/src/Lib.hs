module Lib where

import Input (input)

countJumps :: (Int -> Int) -> [Int] -> Int
countJumps incFn js =
  step 0 0 js
  where
    step :: Int -> Int -> [Int] -> Int
    step count pos jumps =
      if pos < 0 || pos > length jumps - 1
        then count
        else
          let
            (xs, (jump:ys)) = splitAt pos jumps
          in
            step (count + 1) (pos + jump) (xs ++ [incFn jump] ++ ys)

-- Part 1
solution1 :: Int
solution1 = countJumps (\j -> j+1) input

-- Part 2
solution2 :: Int
solution2 = countJumps (\j -> if j >= 3 then j-1 else j+1) input
