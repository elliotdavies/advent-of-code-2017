module Lib where

import Data.List (sortOn, find)

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
data Result = Weight (Int, Int) | Correction Int deriving (Show)

isCorrection :: Result -> Bool
isCorrection (Correction i) = True
isCorrection _ = False

findImbalance :: Node -> [Node] -> Int
findImbalance node nodes =
  case findImbalance' node of
    Correction c -> c
    Weight w -> 0 -- Won't hit this during the puzzle as tree is always imbalanced

  where
    findImbalance' :: Node -> Result
    findImbalance' node =
      let
        branchResults =
          map findImbalance' (findBranches node)

      in
        case find isCorrection branchResults of
          Just c -> c
          Nothing ->
            balance node $ map (\(Weight (w,bw)) -> (w,w+bw)) branchResults

    findBranches :: Node -> [Node]
    findBranches node =
      filter (\n -> name n `elem` (branches node)) nodes

-- Try to balance a node that has the given branch weights
balance :: Node -> [(Int, Int)] -> Result
balance node [] = Weight (weight node, 0)
balance node ws =
  let
    totals = map snd ws

  in
    if maximum totals == minimum totals
      then Weight (weight node, sum totals)
      else Correction (correctImbalance $ sortOn snd ws)

-- Correct an imbalanced list of weights
correctImbalance :: [(Int, Int)] -> Int
correctImbalance ((w,bw):(w',bw'):ws) =
  if bw > bw'
    then w - (bw - bw')
    else if bw < bw'
      then w + (bw' - bw)
      else correctImbalance $ reverse ((w',bw'):ws)

solution2 :: Int
solution2 = findImbalance (findRoot input) input
