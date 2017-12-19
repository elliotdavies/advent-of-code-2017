module Lib where

import Data.List (elemIndex)
import Data.List.Index (imap)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

import Input (input)

data Move
  = Spin Int
  | Exchange Int Int
  | Partner Char Char
  deriving (Show)

type Programs = [Char]

parseInput :: String -> [Move]
parseInput = map parse . splitOn ","
  where
    parse :: String -> Move
    parse (s:ss) =
      case s of
        's' -> Spin (read ss)
        'x' -> let (a:b:_) = splitOn "/" ss in Exchange (read a) (read b)
        'p' -> let (a:sep:b:_) = ss in Partner a b

initPrograms :: Programs
initPrograms = ['a'..'p']

run :: Programs -> [Move] -> Programs
run = foldl step
  where
    step :: Programs -> Move -> Programs
    step ps m =
      case m of
        Spin n -> spin n ps
        Exchange i j -> exchange i j ps
        Partner a b -> partner a b ps

    spin :: Int -> Programs -> Programs
    spin n ps = zipWith const (drop (length ps - n) $ cycle ps) ps

    exchange :: Int -> Int -> Programs -> Programs
    exchange i j ps = imap swap ps
      where
        swap :: Int -> Char -> Char
        swap idx p
          | idx == i = ps !! j
          | idx == j = ps !! i
          | otherwise = p

    partner :: Char -> Char -> Programs -> Programs
    partner a b ps =
      let
        i = fromJust $ elemIndex a ps
        j = fromJust $ elemIndex b ps
      in
        exchange i j ps

solution1 :: Programs
solution1 = run initPrograms (parseInput input)
