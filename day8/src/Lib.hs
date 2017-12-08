module Lib where

import qualified Data.Map as M
import Data.List (sort)

import Input (input)

data Action = Inc | Dec

type InstrStr = (String, String, Integer, String, (String, Integer))
type Instr = (String, Action, Integer, String, (Integer -> Bool))

type Registers = M.Map String Integer
type Result = (Registers, Integer)

-- Parse string-y instructions into a nicer format
parseInstrs :: [InstrStr] -> [Instr]
parseInstrs = map parse
  where
    parse (k, action, val, condK, op) =
      let action' = if action == "inc" then Inc else Dec
      in (k, action', val, condK, parseOp op)

    parseOp (o,v) =
      case o of
        ">" -> (> v)
        "<" -> (< v)
        ">=" -> (>= v)
        "<=" -> (<= v)
        "==" -> (== v)
        "!=" -> (/= v)

-- Populate registers with default values
populateRegs :: [Instr] -> Registers
populateRegs instrs =
  let keys = instrs >>= (\(k,_,_,k',_) -> [k,k'])
  in foldl (\r k -> M.insert k 0 r) M.empty keys

-- Run a single instruction
runSingle :: Result -> Instr -> Result
runSingle (r,high) (k,action,val,condK,condF) =
  case M.lookup condK r of
    Just condV ->
      if condF condV
        then
          let
            r' = case action of
              Inc -> M.adjust ((+) val) k r
              Dec -> M.adjust (flip (-) $ val) k r
          in (r', max (maximum r') high)

        else (r,high)

    Nothing ->
      (r,high)

-- Run many instructions
run :: [InstrStr] -> Result
run instrs =
  let instrs' = parseInstrs instrs
  in foldl runSingle (populateRegs instrs', 0) instrs'

largestValue :: [InstrStr] -> Integer
largestValue = maximum . fst . run

largestSeen :: [InstrStr] -> Integer
largestSeen = snd . run

solution1 :: Integer
solution1 = largestValue input

solution2 :: Integer
solution2 = largestSeen input
