module Lib where

import qualified Data.Map as Map
import Data.Maybe (isNothing, maybe, catMaybes)
import Debug.Trace (trace)

-- Part 1
manhattanDistance :: Integer -> Float
manhattanDistance n =
  let
    root =
      ceiling $ sqrt $ fromIntegral n

    -- Length of a side of the grid
    sideLen =
      if root `mod` 2 /= 0 then root else root + 1

    -- Steps from an axis to the centre of the grid
    stepsFromAxis =
      (fromIntegral (sideLen - 1)) / 2

    -- How far we are from the nearest axis
    offsetFromAxis =
      fromIntegral $ (n - ((sideLen - 2) ^ 2)) `mod` (sideLen - 1)
  in
    stepsFromAxis + (abs (offsetFromAxis - stepsFromAxis))

-- Part 2
data Direction = U | L | D | R deriving (Show)
type Coords = (Integer, Integer)
type Grid = Map.Map Coords Integer


-- Movement helpers
upFrom :: Coords -> Coords
upFrom (x,y) = (x, y-1)

leftFrom :: Coords -> Coords
leftFrom (x,y) = (x-1, y)

downFrom :: Coords -> Coords
downFrom (x,y) = (x, y+1)

rightFrom :: Coords -> Coords
rightFrom (x,y) = (x+1, y)

-- The next direction, given the current coordinates and direction
--  (i.e. do we need to turn?)
nextDir :: Grid -> Coords -> Direction -> Direction
nextDir g cs dir =
  case dir of
    U -> if isNothing (Map.lookup (leftFrom cs) g) then L else U
    L -> if isNothing (Map.lookup (downFrom cs) g) then D else L
    D -> if isNothing (Map.lookup (rightFrom cs) g) then R else D
    R -> if isNothing (Map.lookup (upFrom cs) g) then U else R

-- The values of every cell adjacent to the current one
adjacentValues :: Grid -> Coords -> [Integer]
adjacentValues g (x,y) =
  let
    adjacentCs =
      [(x+1,y), (x-1,y), (x,y+1), (x,y-1), (x+1,y+1), (x+1,y-1), (x-1,y+1), (x-1,y-1)]
  in
    catMaybes $ map (flip Map.lookup $ g) adjacentCs

-- Work out the largest value in the grid after the given one
largestAfter :: Integer -> Integer
largestAfter n =
  let
    (grid, coords) =
      generateGridHelper (Map.singleton (0,0) 1) (0,0) R

  in
    maybe 0 id (Map.lookup coords grid)

  where
    generateGridHelper :: Grid -> Coords -> Direction -> (Grid, Coords)
    generateGridHelper grid coords dir =
      let
        coords' =
          case dir of
            U -> upFrom coords
            L -> leftFrom coords
            D -> downFrom coords
            R -> rightFrom coords

        dir' =
          nextDir grid coords' dir

        val =
          sum $ adjacentValues grid coords'

        grid' =
          Map.insert coords' val grid

      in
        if val > n
          then (grid', coords')
          else generateGridHelper grid' coords' dir'
