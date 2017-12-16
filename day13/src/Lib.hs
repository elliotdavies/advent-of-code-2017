module Lib where

import Data.Maybe (catMaybes, isNothing)

import Input (input)

type Scanner = (Int,Int)

detections :: [Scanner] -> [Maybe Int]
detections = map detect
  where
    detect :: Scanner -> Maybe Int
    detect (depth,range) =
      if depth `mod` (2 * (range - 1)) == 0
        then Just (depth * range)
        else Nothing

severity :: [Scanner] -> Int
severity = sum . catMaybes . detections

delay :: [Scanner] -> Int
delay = try 0
  where
    try ticks scanners =
      if all isNothing (detections scanners)
        then ticks
        else try (ticks + 1) (map (\(d,r) -> (d+1,r)) scanners)

solution1 :: Int
solution1 = severity input

solution2 :: Int
solution2 = delay input
