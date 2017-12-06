module Lib where

import Input (input)
import Debug.Trace (trace)

-- Part 1
countJumps :: [Int] -> Int
countJumps js =
  step 0 0 js

elemAt :: Int -> [a] -> Maybe a
elemAt idx xs =
  safeHead $ filter (\(x,i) -> i == idx) $ zip xs [0..]
  where
    safeHead xs =
      if length xs > 0 then Just (fst $ head xs) else Nothing

step :: Int -> Int -> [Int] -> Int
step count pos jumps =
  let
    jump =
      elemAt pos jumps

    incrementedJumps =
      map fst $ map (\(j,i) -> if i == pos then (j+1,i) else (j,i)) $ zip jumps [0..]

  in
    case jump of
      Nothing ->
        count

      Just j ->
        step (count + 1) (pos + j) incrementedJumps

solution1 :: Int
solution1 = countJumps input
