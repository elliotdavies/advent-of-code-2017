module Lib where

import Data.List (sort, reverse)
import Input (input)

-- Puzzle 1
type Spreadsheet = [[Integer]]

checksum :: Spreadsheet -> Integer
checksum rows =
  let
    diff row =
      let
        highest =
          foldr (\x acc -> if x > acc then x else acc) 0 row

        lowest =
          foldr (\x acc -> if x < acc then x else acc) highest row

      in
        highest - lowest

  in
    sum $ map diff rows

solution1 :: Integer
solution1 = checksum input

-- Puzzle 2
checksumEvenlyDivisibles :: Spreadsheet -> Integer
checksumEvenlyDivisibles rows =
  let
    diff row =
      let
        evenlyDivisibles =
          [(x,y) | x <- row, y <- row, x /= y, x `mod` y == 0 ]

      in
        (\(x,y) -> x `div` y) $ head evenlyDivisibles

  in
    sum $ map diff rows

solution2 :: Integer
solution2 = checksumEvenlyDivisibles input
