module Lib where

import Input (input)
import Data.List (sort)
import qualified Data.Set as Set

type SetInsertFn a = a -> Set.Set a -> Set.Set a

validPassphrases :: SetInsertFn String -> [String] -> Int
validPassphrases insertFn =
  length . filter (unique . words)
  where
    unique ws =
      Set.size (foldr insertFn Set.empty ws) == length ws

-- Part 1
passphrasesWithoutDuplicates :: [String] -> Int
passphrasesWithoutDuplicates = validPassphrases Set.insert

solution1 :: Int
solution1 = passphrasesWithoutDuplicates input

-- Part 2
passphrasesWithoutPalindromes :: [String] -> Int
passphrasesWithoutPalindromes = validPassphrases reorderAndInsert
  where
    reorderAndInsert ws = Set.insert (sort ws)

solution2 :: Int
solution2 = passphrasesWithoutPalindromes input
