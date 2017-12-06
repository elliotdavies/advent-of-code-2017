module Lib where

-- Part 1
manhattanDistance :: Integer -> Float
manhattanDistance n =
  let
    root =
      ceiling $ sqrt $ fromIntegral n

    -- Length of a side of the grid
    sideLen =
      if root `mod` 2 /= 0 then root else root + 1

    -- Steps from an axis to the centre of the grid
    stepsFromAxis =
      (fromIntegral (sideLen - 1)) / 2

    -- How far we are from the nearest axis
    offsetFromAxis =
      fromIntegral $ (n - ((sideLen - 2) ^ 2)) `mod` (sideLen - 1)
  in
    stepsFromAxis + (abs (offsetFromAxis - stepsFromAxis))
