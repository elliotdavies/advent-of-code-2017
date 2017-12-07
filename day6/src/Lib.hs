module Lib where

-- import qualified Data.Set as Set
import Data.List.Index (ifoldl, setAt, modifyAt, ifindIndex)

import Input (input)

redist :: [[Int]] -> [Int] -> (Int, Int)
redist history banks =
  let
    (i, blocks) =
      ifoldl (\acc i b -> if b > snd acc then (i,b) else acc) (0,0) banks

    banks' =
      dist blocks (i+1) $ setAt i 0 banks

  in
    case ifindIndex (\i b -> b == banks') history of
      Just i ->
        (length history, length history - i + 1)

      Nothing ->
        redist (history ++ [banks]) banks'

  where
    dist 0 _ banks = banks
    dist blocks i banks =
      dist (blocks-1) (i+1) $ modifyAt (i `mod` length banks) (+1) banks

solution1 :: Int
solution1 =
  fst $ redist [input] input

solution2 :: Int
solution2 =
  snd $ redist [input] input
