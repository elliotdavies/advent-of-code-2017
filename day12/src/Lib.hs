module Lib where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Debug.Trace (trace)

import Input (input)

parseInput :: [(Int, [Int])] -> M.Map Int [Int]
parseInput = foldr (uncurry M.insert) M.empty

-- Recursively count connections from a given point
connectionsFrom :: Int -> M.Map Int [Int] -> S.Set Int
connectionsFrom n pipes = countHelper S.empty n
  where
    countHelper :: S.Set Int -> Int -> S.Set Int
    countHelper set from =
      let
        set' = S.insert from set
        connections = fromJust $ M.lookup from pipes
        unseen = filter (not . (flip S.member $ set')) connections
      in
        foldl (\acc c -> S.union acc $ countHelper acc c) set' unseen

-- Number of connections from 0
solution1 :: Int
solution1 = S.size $ connectionsFrom 0 $ parseInput input

-- Number of distinct groups seen
solution2 :: Int
solution2 =
  snd $ M.foldWithKey foldHelper (S.empty, 0) pipes
  where
    pipes = parseInput input

    foldHelper :: Int -> [Int] -> (S.Set Int, Int) -> (S.Set Int, Int)
    foldHelper k _ (seen, groups) =
      if S.member k seen
        then (seen, groups)
        else (S.union seen (connectionsFrom k pipes), groups + 1)
