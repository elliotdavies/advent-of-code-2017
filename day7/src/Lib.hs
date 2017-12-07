module Lib where

import Data.List (union)

import Input (input)

findRoot :: [(String, Int, [String])] -> String
findRoot nodes =
  let
    names = map (\(n,_,_) -> n) nodes
    pointedTo = nodes >>= (\(_,_,xs) -> xs)

  in
    head $ filter (not . (\n -> n `elem` pointedTo)) names

solution1 :: String
solution1 = findRoot input
