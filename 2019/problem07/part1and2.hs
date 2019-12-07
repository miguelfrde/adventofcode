{-# Language RecordWildCards #-}

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.List.Split
import Data.Vector (Vector, (!), (//), toList, fromList)
import System.Environment

type Memory = Vector Int

data MemoryMessVm = MemoryMessVm {
    pc :: Int,
    mem :: Memory,
    stdout :: [Int],
    stdin :: [Int],
    halt :: Bool
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
    JmpNz a b -> evalJmp (/=0) a b
    JmpZero a b -> evalJmp (==0) a b
    CmpLe a b out -> evalCmp (<) a b out
    CmpEq a b out -> evalCmp (==) a b out
  where
    op = evalOperand vm
    evalJmp f a b
      | f . op $ a = vm { pc = op b }
      | otherwise = vm { pc = pc + 3 }
    evalCmp f a b out =
      let val = if f (op a) (op b) then 1 else 0
      in vm { pc = pc + 4, mem = mem // [(out, val)] }

parseNumberList :: String -> [Int]
parseNumberList = map toInt . splitOn ","
  where toInt = read::String->Int

solve :: [Int] -> String -> Int
solve xs input = maximum . map (singleStep vm) $ (permutations xs)
  where startup = fromList . parseNumberList $ input
        vm = MemoryMessVm { pc = 0, mem = startup, stdout = [], stdin = [], halt = False }

data Amplifier = Amplifier {
    phase :: Int,
    machine :: MemoryMessVm,
    label :: Char
}

outputAmp :: Amplifier -> Int
outputAmp = head . stdout . machine

executeAmp :: Amplifier -> [Int] -> Amplifier
executeAmp a is = a{ machine = executeUntilEffect v{ stdin = stdin v ++ is }}
  where v = machine a

start :: Amplifier -> Amplifier
start a = executeAmp a [phase a]

ampDone :: Amplifier -> Bool
ampDone = halt . machine

executeUntilEffect :: MemoryMessVm -> MemoryMessVm
executeUntilEffect vm@MemoryMessVm{..} = case current of
    Halt -> vm { halt = True }
    Input out -> case stdin of
      [] -> vm
      (x:xs) ->  executeUntilEffect vm { pc = pc + 2, mem = mem // [(out, x)] , stdin = xs }
    Output o -> vm { pc = pc + 2, stdout = (evalOperand vm o):stdout}
    _ -> executeUntilEffect . eval vm $ current
  where current = currentInstruction vm

singleStep :: MemoryMessVm -> [Int] -> Int
singleStep vm phases = advance 0 amplifiers
  where amplifiers = map (start . newAmplifier) $ zip phases ['A'..'Z']
        newAmplifier (p, c) = Amplifier { label = c, phase = p, machine = vm}
        advance i amps@(a:as)
          | (and . map ampDone $ as) && label a == 'E' = head . stdout . machine $ updatedAmp
          | otherwise = advance (outputAmp updatedAmp) (as ++ [updatedAmp])
          where updatedAmp = executeAmp a [i]


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve [0,1,2,3,4] $ content
  putStrLn . show . solve [5,6,7,8,9] $ content
