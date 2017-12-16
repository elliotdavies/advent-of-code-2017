module Lib where

import qualified Data.Map as M

import Input (input)

data Dir = Up | Down deriving (Show,Eq)
data Scanner = Scanner { depth :: Int, range :: Int, pos :: Int, dir :: Dir } deriving (Show)

toScanners :: [(Int,Int)] -> [Scanner]
toScanners = map (\(d,r) -> Scanner { depth = d, range = r, pos = 0, dir = Down })

withGaps :: [Scanner] -> [Maybe Scanner]
withGaps scanners = foldr fillGaps [Just (last scanners)] $ zip <*> tail $ scanners
  where
    fillGaps :: (Scanner, Scanner) -> [Maybe Scanner] -> [Maybe Scanner]
    fillGaps (s1,s2) acc =
      let blanks = replicate (depth s2 - depth s1 - 1) Nothing
      in Just s1 : (blanks ++ acc)

initialise :: [(Int,Int)] -> [Maybe Scanner]
initialise = withGaps . toScanners

tick :: [Maybe Scanner] -> [Maybe Scanner]
tick = map (fmap tickHelper)
  where
    tickHelper :: Scanner -> Scanner
    tickHelper scanner
      | pos scanner == 0                  = scanner { pos = 1, dir = Down }
      | pos scanner == range scanner - 1  = scanner { pos = range scanner - 2, dir = Up }
      | otherwise =
          scanner { pos = if dir scanner == Up then pos scanner - 1 else pos scanner + 1 }

severity :: [Maybe Scanner] -> Int
severity = severityHelper 0 0
  where
    severityHelper :: Int -> Int -> [Maybe Scanner] -> Int
    severityHelper i total scanners =
      let
        i' = i + 1
        scanners' = tick scanners
      in
        if i == length scanners
          then total
          else case scanners !! i of
            Just scanner ->
              if pos scanner == 0
                then severityHelper i' (total + (depth scanner * range scanner)) scanners'
                else severityHelper i' total scanners'

            Nothing ->
              severityHelper i' total scanners'

delay :: [Maybe Scanner] -> Int
delay = try 0 . map (fmap (\s -> s { depth = depth s + 1 }))
  where
    try ticks scanners =
      if severity scanners == 0
        then ticks
        else try (ticks + 1) (tick scanners)

solution1 :: Int
solution1 = severity $ initialise input

solution2 :: Int
solution2 = delay $ initialise input
