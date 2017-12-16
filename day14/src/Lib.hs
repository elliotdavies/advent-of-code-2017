module Lib where

import Data.Char (ord)
import Hash (knotHash)
import Text.Printf (printf)

hexToBinary :: String -> String
hexToBinary = concatMap (printf "%04b" . translate)
  where
    translate :: Char -> Int
    translate c
      | c <= '9'  = ord c - 48
      | otherwise = ord c - 87

hashInputs :: String -> [String]
hashInputs key = map (\d -> key ++ "-" ++ show d) [0..127]

hashes :: String -> [String]
hashes = map knotHash . hashInputs

usedSquares :: String -> Int
usedSquares = sum . map (length . filter (== '1') . hexToBinary) . hashes

solution1 :: Int
solution1 = usedSquares "vbqugkhl"
