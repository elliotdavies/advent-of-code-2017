module Lib where

import Data.List.Split (splitOn)

import Debug.Trace (trace)

-- import Input (input)



{-
parseItem :: Tokens -> (Item, Tokens)
parseItem takes an input string
  - If starts with <, find > and return (Garbage, leftover tokens)
  - If starts with {, parseGroup and return (Group items, leftover tokens)

parseGroup :: Tokens -> [Item]
parseGroup takes an input string; need to parse multiple items
  - parseItem gives a result and leftover tokens
  - if leftover token is '}' return list of results
  - if leftover token is ',' discard and parseItem again
-}

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

parseItem :: Tokens -> (Item, Tokens)
parseItem [] = (Nil, [])
parseItem (t:ts) =
  case trace ("parseItem: " ++ show t) t of
    '<' ->
      let ts' = dropGarbage ts
      in (Garbage, ts')

    '{' ->
      let (innerItems, ts') = parseGroupInner ts
      in (Group innerItems, ts')

parseGroupInner :: Tokens -> ([Item], Tokens)
parseGroupInner [] = ([Nil], [])
parseGroupInner (t:ts) =
  case t of
    '}' ->
      ([], ts)

    ',' ->
      parseGroupInner ts

    _ ->
      let
        (item, ts') = parseItem ts
        (items, ts'') = parseGroupInner ts'
      in
        (item:items, ts'')

parseStream :: Tokens -> Item
parseStream = fst . parseItem
