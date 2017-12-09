module Lib where

import Data.List (sort, reverse)
import Input (input)

-- Puzzle 1
type Spreadsheet = [[Integer]]

checksum :: Spreadsheet -> Integer
checksum = sum . map diff
  where
    diff row = maximum row - minimum row

solution1 :: Integer
solution1 = checksum input

-- Puzzle 2
checksumEvenlyDivisibles :: Spreadsheet -> Integer
checksumEvenlyDivisibles = sum . map diff
  where
    diff row =
      let
        evenlyDivisibles = [(x,y) | x <- row, y <- row, x /= y, x `mod` y == 0]

      in
        (\(x,y) -> x `div` y) $ head evenlyDivisibles

solution2 :: Integer
solution2 = checksumEvenlyDivisibles input
