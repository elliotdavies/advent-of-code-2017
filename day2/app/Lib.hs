module Lib where

import Data.List (sort)
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
