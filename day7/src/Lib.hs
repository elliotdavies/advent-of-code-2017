module Lib where

import Data.List (sort, sortOn)
import Debug.Trace (trace)

import Input (input)

type Node = (String, Int, [String])

name :: Node -> String
name (n,_,_) = n

weight :: Node -> Int
weight (_,w,_) = w

branches :: Node -> [String]
branches (_,_,ns) = ns

-- Part 1
findRoot :: [Node] -> Node
findRoot nodes =
  let
    pointedTo = nodes >>= branches

  in
    head $ filter (not . (flip elem $ pointedTo) . name) nodes

solution1 :: String
solution1 = name $ findRoot input

-- Part 2
findImbalance :: Node -> [Node] -> Int
findImbalance node nodes =
  let
    weights =
      zip (map weight $ findBranches (branches node)) (branchWeights (branches node))

  in
    correction $ sortOn snd weights

  where
    findBranches :: [String] -> [Node]
    findBranches names =
      filter (\n -> name n `elem` names) nodes

    branchWeights :: [String] -> [Int]
    branchWeights names =
      map (\n -> weight n + sum (branchWeights $ branches n)) $ findBranches names

    correction :: [(Int,Int)] -> Int
    correction [] = 0
    correction (_:[]) = 0
    correction ((w,bw):(w',bw'):ws) =
      if bw > bw'
        then w - (bw - bw')
        else if bw < bw'
          then w + (bw' - bw)
          else correction $ reverse ((w',bw'):ws)

solution2 :: Int
solution2 = findImbalance (findRoot input) input
