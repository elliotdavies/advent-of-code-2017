module Lib where

import Data.Char (ord)
import Data.List.Index (imap, ifoldr)
import qualified Data.Set as S
import Hash (knotHash)
import Text.Printf (printf)

type BitGrid = [[Bool]]
type Coords = (Int,Int)
type CoordsSet = S.Set Coords

hexToBinary :: String -> String
hexToBinary = concatMap (printf "%04b" . translate)
  where
    translate :: Char -> Int
    translate c
      | c <= '9'  = ord c - 48
      | otherwise = ord c - 87

hashInputs :: String -> [String]
hashInputs key = map (\d -> key ++ "-" ++ show d) [0..127]

grid :: String -> BitGrid
grid = map (toBooleans . hexToBinary . knotHash) . hashInputs
  where
    toBooleans :: [Char] -> [Bool]
    toBooleans = map (== '1')

usedSquares :: BitGrid -> Int
usedSquares = sum . map (length . filter id)

-- Part 2
uniqueRegions :: BitGrid -> Int
uniqueRegions = fst . countRegions 0 . S.unions . imap rowToSet
  where
    rowToSet :: Int -> [Bool] -> CoordsSet
    rowToSet rowIdx = ifoldr (\i el acc -> if el then S.insert (i, rowIdx) acc else acc) S.empty

    countRegions :: Int -> CoordsSet -> (Int, CoordsSet)
    countRegions total s
      | S.null s = (total, s)
      | otherwise = countRegions (total + 1) (eraseFrom (S.elemAt 0 s) s)
      where
        eraseFrom :: Coords -> CoordsSet -> CoordsSet
        eraseFrom (x,y) s
          | S.notMember (x,y) s = s
          | otherwise = foldr eraseFrom (S.delete (x,y) s) [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

solution1 :: Int
solution1 = usedSquares $ grid "vbqugkhl"

solution2 :: Int
solution2 = uniqueRegions $ grid "vbqugkhl"
