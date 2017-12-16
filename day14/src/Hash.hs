{- Copied from day 10 and tweaked to accept a string -}

module Hash where

import Data.Bits (xor)
import Data.Char (ord, chr)
import Data.List (splitAt, intercalate)
import Data.List.Split (chunksOf)
import Text.Printf (printf)

runHash :: Int -> Int -> [Int] -> [Int] -> [Int]
runHash _ _ xs [] = xs
runHash pos skip xs (l:lens) =
  if pos + l <= length xs
    -- Don't need to cycle
    then
      let
        (toRev,rest) = splitAt l $ drop pos xs
        xs' = take pos xs ++ reverse toRev ++ rest
        pos' = (pos + l + skip) `mod` length xs
      in
        runHash pos' (skip + 1) xs' lens

    -- Need to cycle
    else
      let
        fromPos = length xs - pos
        fromFront = l - fromPos
        (rest,end) = splitAt pos xs
        (front,middle) = splitAt fromFront rest
        rev = reverse $ end ++ front
        (end',front') = splitAt (length end) rev
        xs' = front' ++ middle ++ end'
        pos' = (pos + l + skip) `mod` length xs
      in
        runHash pos' (skip + 1) xs' lens

knotHash :: String -> String
knotHash =
  concatMap (printf "%02x" . foldr xor 0)
  . chunksOf 16
  . runHash 0 0 [0..255]
  . concat . replicate 64
  . (++ [17,31,73,47,23])
  . map ord-- . intercalate "," . map show
