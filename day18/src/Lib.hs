module Lib where

import qualified Data.Map    as M
import qualified Data.Vector as V
import           Text.Read   (readMaybe)

import           Input       (input)

data Arg
  = Reg Char
  | Val Int
  deriving (Show)

data Instruction
  = SetFreq Arg
  | GetFreq Arg
  | Fn (Int -> Int -> Int) Arg Arg
  | Jump Arg Arg

data Program = P { freq :: Maybe Int, registers :: M.Map Char Int }

parseInput :: String -> V.Vector Instruction
parseInput = V.fromList . map parse . lines
  where
    parseArg :: String -> Arg
    parseArg s =
      case readMaybe s of
        Just i  -> Val i
        Nothing -> Reg $ head s

    parse :: String -> Instruction
    parse s =
      case words s of
        ["snd",x]   -> SetFreq (parseArg x)
        ["rcv",x]   -> GetFreq (parseArg x)
        ["jgz",x,y] -> Jump (parseArg x) (parseArg y)
        ["set",x,y] -> Fn (flip const) (parseArg x) (parseArg y)
        ["add",x,y] -> Fn (+) (parseArg x) (parseArg y)
        ["mul",x,y] -> Fn (*) (parseArg x) (parseArg y)
        ["mod",x,y] -> Fn mod (parseArg x) (parseArg y)

-- Run until the program ends or a valid GetFreq is encountered
run :: V.Vector Instruction -> Maybe Int
run v = step P { freq = Nothing, registers = M.empty } 0
  where
    step :: Program -> Int -> Maybe Int
    step p i =
      case v V.!? i of
        Nothing -> freq p -- Out of bounds

        Just instr ->
          case instr of
            SetFreq x ->
              step p { freq = Just (val x) } (i + 1)

            GetFreq x ->
              if val x /= 0 then freq p else step p (i + 1)

            Jump x y  ->
              if val x > 0 then step p (i + val y) else step p (i + 1)

            Fn f x y  ->
              let registers' = M.insert (reg x) (f (val x) (val y)) (registers p)
              in step p { registers = registers' } (i + 1)

      where
        val :: Arg -> Int
        val (Val i) = i
        val (Reg r) = M.findWithDefault 0 r (registers p)

        reg :: Arg -> Char
        reg (Reg r) = r

solution1 :: Maybe Int
solution1 = run $ parseInput input
