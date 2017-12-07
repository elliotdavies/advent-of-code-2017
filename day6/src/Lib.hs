module Lib where

import qualified Data.Set as Set
import Data.List.Index (ifoldl, setAt, modifyAt)

import Input (input)

redist :: Set.Set [Int] -> [Int] -> Int
redist history banks =
  let
    (i, blocks) =
      ifoldl (\acc i b -> if b > snd acc then (i,b) else acc) (0,0) banks

    banks' =
      dist blocks (i+1) $ setAt i 0 banks

  in
    if Set.member banks' history
      then Set.size history + 1
      else redist (Set.insert banks history) banks'
  where
    dist 0 _ banks = banks
    dist blocks i banks =
      dist (blocks-1) (i+1) $ modifyAt (i `mod` length banks) (+1) banks

solution1 :: Int
solution1 =
  redist (Set.singleton input) input
