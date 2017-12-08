module Lib where

import qualified Data.Map as M
import Data.List (sort)

import Input (input)

-- Part 1
data Action = Inc | Dec

type InstrStr = (String, String, Integer, String, (String, Integer))
type Instr = (String, Action, Integer, String, (Integer -> Bool))

type Registers = M.Map String Integer

parseInstrs :: [InstrStr] -> [Instr]
parseInstrs = map parse
  where
    parse :: InstrStr -> Instr
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

populateRegs :: [Instr] -> Registers
populateRegs instrs =
  let
    keys = instrs >>= (\(k,_,_,k',_) -> [k,k'])
  in
    foldl (\r k -> M.insert k 0 r) M.empty keys

run :: [Instr] -> Registers
run instrs = foldl runSingle (populateRegs instrs) instrs

runSingle :: Registers -> Instr -> Registers
runSingle r (k,action,val,condK,condF) =
  case M.lookup condK r of
    Just condV ->
      if condF condV
        then case action of
          Inc -> M.adjust ((+) val) k r
          Dec -> M.adjust (flip (-) $ val) k r
        else r

    Nothing ->
      r

largestValue :: [InstrStr] -> Integer
largestValue = head . reverse . sort . M.elems . run . parseInstrs

solution1 :: Integer
solution1 = largestValue input
