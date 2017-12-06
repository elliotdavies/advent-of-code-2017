module Lib where

import Input (input)
import Debug.Trace (trace)

-- Should be able to cut this out and save some time
elemAt :: Int -> [a] -> Maybe a
elemAt idx xs =
  safeHead $ filter (\(x,i) -> i == idx) $ zip xs [0..]
  where
    safeHead xs =
      if length xs > 0 then Just (fst $ head xs) else Nothing

countJumps :: (Int -> Int) -> [Int] -> Int
countJumps incFn js =
  step 0 0 js
  where
    step :: Int -> Int -> [Int] -> Int
    step count pos jumps =
      let
        jump =
          elemAt pos jumps

        incrementedJumps =
          map fst $ map (\(j,i) -> if i == pos then ((incFn j),i) else (j,i)) $ zip jumps [0..]

      in
        case jump of
          Nothing ->
            count

          Just j ->
            step (count + 1) (pos + j) incrementedJumps

-- Part 1
solution1 :: Int
solution1 = countJumps (\j -> j+1) input

-- Part 2
solution2 :: Int
solution2 = countJumps (\j -> if j >= 3 then j-1 else j+1) input
