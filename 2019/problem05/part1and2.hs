{-# Language RecordWildCards #-}

import qualified Data.Map as Map
import Data.List.Split
import Data.Vector (Vector, (!), (//), toList, fromList)
import System.Environment

type Memory = Vector Int

data MemoryMessVm = MemoryMessVm {
    pc :: Int,
    mem :: Memory,
    stdout :: [Int],
    stdin :: [Int]
}

data Operand = Ref Int | Val Int deriving (Show, Eq)

data Instruction =
  Add Operand Operand Int
  | Mul Operand Operand Int
  | Input Int
  | Output Operand
  | JmpZero Operand Operand
  | JmpNz Operand Operand
  | CmpLe Operand Operand Int
  | CmpEq Operand Operand Int
  | Halt
  deriving (Show, Eq)

evalOperand :: MemoryMessVm -> Operand -> Int
evalOperand vm@MemoryMessVm{..} (Ref x) = mem ! x
evalOperand vm@MemoryMessVm{..} (Val x) = x

currentValue :: MemoryMessVm -> Int -> Int
currentValue vm@MemoryMessVm{..} i = mem ! (pc + i)

operand :: Int -> Int -> Operand
operand 0 x = Ref x
operand 1 x = Val x

currentInstruction :: MemoryMessVm -> Instruction
currentInstruction vm = case opCode of
  1 -> Add (operand' 1) (operand' 2) (val 3)
  2 -> Mul (operand' 1) (operand' 2) (val 3)
  3 -> Input (val 1)
  4 -> Output (operand' 1)
  5 -> JmpNz (operand' 1) (operand' 2)
  6 -> JmpZero (operand' 1) (operand' 2)
  7 -> CmpLe (operand' 1) (operand' 2) (val 3)
  8 -> CmpEq (operand' 1) (operand' 2) (val 3)
  99 -> Halt
  x -> error ("Unknown op " ++ show x)
  where opCode = val 0 `mod` 100
        param i x = x `div` (10^i) `mod` 10
        val = currentValue vm
        operand' i = operand (param (i+1) (val 0)) (val i)

eval :: MemoryMessVm -> Instruction -> MemoryMessVm
eval vm@MemoryMessVm{..} inst  = case inst of
    Add a b out  -> vm{ pc = pc + 4, mem = mem // [(out, op a + op b)] }
    Mul a b out  -> vm{ pc = pc + 4, mem = mem // [(out, op a * op b)] }
    Input out    -> vm{ pc = pc + 2, mem = mem // [(out, head stdin)] , stdin = tail stdin}
    Output out -> vm{ pc = pc + 2, stdout = (op out):stdout }
    JmpNz a b -> evalJmp (/=0) a b
    JmpZero a b -> evalJmp (==0) a b
    CmpLe a b out -> evalCmp (<) a b out
    CmpEq a b out -> evalCmp (==) a b out
    Halt -> vm
  where
    op = evalOperand vm
    evalJmp f a b
      | f . op $ a = vm { pc = op b }
      | otherwise = vm { pc = pc + 3 }
    evalCmp f a b out =
      let val = if f (op a) (op b) then 1 else 0
      in vm { pc = pc + 4, mem = mem // [(out, val)] }

execute :: MemoryMessVm -> MemoryMessVm
execute vm = case current of
    Halt -> vm
    _ -> execute . eval vm $ current
  where current = currentInstruction vm

parseNumberList :: String -> [Int]
parseNumberList = map toInt . splitOn ","
  where toInt = read::String->Int

solve :: Int -> String -> [Int]
solve i input = stdout . execute $ vm
  where startup = fromList . parseNumberList $ input
        vm = MemoryMessVm { pc = 0, mem = startup, stdout = [], stdin = [i] }

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve 1 $ content
  putStrLn . show . solve 5 $ content
