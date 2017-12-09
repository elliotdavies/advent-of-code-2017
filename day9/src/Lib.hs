module Lib where

import Control.Arrow (first)

import Input (input)

data Item
  = Group [Item]
  | Garbage Tokens
  | Nil
  deriving (Show, Eq)

type Tokens = String

separateGarbage :: Tokens -> (Tokens, Tokens)
separateGarbage [] = ([],[])
separateGarbage (t:ts)
  | t == '>' = ([],ts)
  | t == '!' = separateGarbage $ drop 1 ts
  | otherwise =
    let (garbage, ts') = separateGarbage ts
    in (t:garbage, ts')

parseItems :: Tokens -> ([Item], Tokens)
parseItems [] = ([],[])
parseItems (t:ts) =
  case t of
    '<' ->
      let
        (garbage, ts') = separateGarbage ts
        (items, ts'') = parseItems ts'
      in
        ((Garbage garbage):items, ts'')

      -- first ((:) Garbage) $ parseItems $ separateGarbage ts

    '{' ->
      let
        (innerItems, ts') = parseItems ts
        (adjacentItems, ts'') = parseItems ts'
      in
        ((Group innerItems):adjacentItems, ts'')

    ',' ->
      parseItems ts

    '}' ->
      ([], ts)

-- Part 1
scoreGroups :: Int -> [Item] -> Int
scoreGroups n = sum . map (score n)
  where
    score :: Int -> Item -> Int
    score n (Group items) = n + scoreGroups (n+1) items
    score _ _ = 0

solution1 :: Int
solution1 = scoreGroups 1 $ fst $ parseItems input

-- Part 2
countGarbage :: [Item] -> Int
countGarbage = sum . map score
  where
    score :: Item -> Int
    score (Group items) = countGarbage items
    score (Garbage g) = length g

solution2 :: Int
solution2 = countGarbage $ fst $ parseItems input
