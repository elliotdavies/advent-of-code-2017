module Lib where

import qualified Data.Map as M

import Input (input)

data Dir = Up | Down deriving (Show,Eq)
data Scanner = Scanner { depth :: Int, range :: Int, pos :: Int, dir :: Dir } deriving (Show)

initialise :: [(Int,Int)] -> [Scanner]
initialise = map (\(d,r) -> Scanner { depth = d, range = r, pos = 0, dir = Down })

withGaps :: [Scanner] -> [Maybe Scanner]
withGaps scanners = foldl fillGaps [Just (last scanners)] $ zip <*> tail $ scanners
  where
    fillGaps :: [Maybe Scanner] -> (Scanner, Scanner) -> [Maybe Scanner]
    fillGaps acc (s1,s2) =
      let blanks = replicate (depth s2 - depth s1 - 1) Nothing
      in Just s1 : (blanks ++ acc)

tick :: [Maybe Scanner] -> [Maybe Scanner]
tick = map tickHelper
  where
    tickHelper :: Maybe Scanner -> Maybe Scanner
    tickHelper Nothing = Nothing
    tickHelper (Just scanner)
      | pos scanner == 0                  = Just scanner { pos = 1, dir = Down }
      | pos scanner == depth scanner - 1  = Just scanner { pos = depth scanner - 2, dir = Up }
      | otherwise =
          Just scanner { pos = if dir scanner == Up then pos scanner - 1 else pos scanner + 1 }

severity :: [(Int,Int)] -> Int
severity scanners =
  severityHelper 0 $ withGaps $ initialise scanners
  where
    severityHelper :: Int -> Int -> [Maybe Scanner] -> Int
    severityHelper pos total scanners =
      if pos == length scanners
        then total
        else case scanners !! pos of
          Just scanner ->
            if pos scanner == 0
              then severityHelper (pos + 1) (total + (depth scanner * range scanner)) (tick scanners)
              else severityHelper (pos + 1) total (tick scanners)

          Nothing ->
            severityHelper (pos + 1) total (tick scanners)
