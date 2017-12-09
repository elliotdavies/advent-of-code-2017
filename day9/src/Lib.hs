module Lib where

import Control.Arrow (first)

import Input (input)

data Item
  = Group [Item]
  | Garbage
  | Nil
  deriving (Show, Eq)

type Tokens = String

dropGarbage :: Tokens -> Tokens
dropGarbage [] = []
dropGarbage (t:ts)
  | t == '>' = ts
  | t == '!' = dropGarbage $ drop 1 ts
  | otherwise = dropGarbage ts

parseItems :: Tokens -> ([Item], Tokens)
parseItems [] = ([],[])
parseItems (t:ts) =
  case t of
    '<' ->
      first ((:) Garbage) $ parseItems $ dropGarbage ts

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

parseScore :: Int -> [Item] -> Int
parseScore n = sum . map (score n)
  where
    score :: Int -> Item -> Int
    score n (Group items) = n + parseScore (n+1) items
    score _ _ = 0

-- Part 1
solution1 :: Int
solution1 = parseScore 1 $ fst $ parseItems input
