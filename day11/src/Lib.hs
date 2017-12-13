module Lib where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Math.Geometry.Grid (distance, neighbour)
import Math.Geometry.Grid.Hexagonal2 (UnboundedHexGrid(..))
import Math.Geometry.Grid.HexagonalInternal2 (HexDirection(..))

import Input (input)

parseInput :: String -> [HexDirection]
parseInput = map parse . splitOn ","
  where
    parse p =
      case p of
        "n"  -> North
        "ne" -> Northeast
        "nw" -> Northwest
        "s"  -> South
        "se" -> Southeast
        "sw" -> Southwest

move :: UnboundedHexGrid -> (Int, Int) -> HexDirection -> (Int, Int)
move g cs d = fromJust $ neighbour g cs d

solution1 :: Int
solution1 =
  let
    g = UnboundedHexGrid
    start = (0,0)
  in
    distance g start . foldl (move g) start $ parseInput input

solution2 :: Int
solution2 =
  let
    g = UnboundedHexGrid
    start = (0,0)
  in
    maximum . map (distance g start) . scanl (move g) start $ parseInput input
