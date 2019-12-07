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

data Argument = Ref Int | Val Int deriving (Show, Eq)

data Instruction =
  Add Argument Argument Int
  | Mul Argument Argument Int
  | Input Int
  | Output Argument
  | JmpZero Argument Argument
  | JmpNz Argument Argument
  | CmpLe Argument Argument Int
  | CmpEq Argument Argument Int
  | Halt
  deriving (Show, Eq)

evalArgument :: MemoryMessVm -> Argument -> Int
evalArgument vm@MemoryMessVm{..} (Ref x) = mem ! x
evalArgument vm@MemoryMessVm{..} (Val x) = x

currentValue :: MemoryMessVm -> Int -> Int
currentValue vm@MemoryMessVm{..} i = mem ! (pc + i)

arg :: Int -> Int -> Argument
arg 0 x = Ref x
arg 1 x = Val x

currentInstruction :: MemoryMessVm -> Instruction
currentInstruction vm = case opCode of
  1 -> Add (arg' 1) (arg' 2) (val 3)
  2 -> Mul (arg' 1) (arg' 2) (val 3)
  3 -> Input (val 1)
  4 -> Output (arg' 1)
  5 -> JmpNz (arg' 1) (arg' 2)
  6 -> JmpZero (arg' 1) (arg' 2)
  7 -> CmpLe (arg' 1) (arg' 2) (val 3)
  8 -> CmpEq (arg' 1) (arg' 2) (val 3)
  99 -> Halt
  x -> error ("Unknown op " ++ show x)
  where opCode = val 0 `mod` 100
        param i x = x `div` (10^i) `mod` 10
        val = currentValue vm
        arg' i = arg (param (i+1) (val 0)) (val i)

evalOp :: MemoryMessVm -> Instruction -> MemoryMessVm
evalOp vm@MemoryMessVm{..} inst  = case inst of
    Add a b out  -> vm{ pc = pc + 4, mem = mem // [(out, op a + op b)] }
    Mul a b out  -> vm{ pc = pc + 4, mem = mem // [(out, op a * op b)] }
    JmpNz a b -> evalJmp (/=0) a b
    JmpZero a b -> evalJmp (==0) a b
    CmpLe a b out -> evalCmp (<) a b out
    CmpEq a b out -> evalCmp (==) a b out
  where
    op = evalArgument vm
    evalJmp f a b
      | f . op $ a = vm { pc = op b }
      | otherwise = vm { pc = pc + 3 }
    evalCmp f a b out =
      let val = if f (op a) (op b) then 1 else 0
      in vm { pc = pc + 4, mem = mem // [(out, val)] }


executeUntilEffect :: MemoryMessVm -> MemoryMessVm
executeUntilEffect vm@MemoryMessVm{..} = case current of
    Halt -> vm { halt = True }
    Input out -> case stdin of
      [] -> vm
      (x:xs) ->  executeUntilEffect vm { pc = pc + 2, mem = mem // [(out, x)] , stdin = xs }
    Output o -> vm { pc = pc + 2, stdout = (evalArgument vm o):stdout}
    _ -> executeUntilEffect . evalOp vm $ current
  where current = currentInstruction vm

data Amplifier = Amplifier {
    phase :: Int,
    machine :: MemoryMessVm
}

outputAmp :: Amplifier -> Int
outputAmp = head . stdout . machine

executeAmp :: Amplifier -> Int -> Amplifier
executeAmp amp input = amp{ machine = updatedVm }
  where vm = machine amp
        updatedVm = executeUntilEffect vm{ stdin = stdin vm ++ [input] }

start :: Amplifier -> Amplifier
start a = executeAmp a (phase a)

ampDone :: Amplifier -> Bool
ampDone = halt . machine

singleStep :: MemoryMessVm -> [Int] -> Int
singleStep vm phases = advance 0 amplifiers
  where amplifiers = map (start . newAmplifier) $ phases
        newAmplifier p = Amplifier { phase = p, machine = vm }
        advance i amps@(a:as)
          | allDone as = head . stdout . machine $ updatedAmp
          | otherwise = advance (outputAmp updatedAmp) (as ++ [updatedAmp])
          where updatedAmp = executeAmp a i
                allDone as = and . map ampDone $ as

parseNumberList :: String -> [Int]
parseNumberList = map toInt . splitOn ","
  where toInt = read::String->Int

solve :: [Int] -> String -> Int
solve xs input = maximum . map (singleStep vm) $ (permutations xs)
  where startup = fromList . parseNumberList $ input
        vm = MemoryMessVm { pc = 0, mem = startup, stdout = [], stdin = [], halt = False }

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve [0,1,2,3,4] $ content
  putStrLn . show . solve [5,6,7,8,9] $ content
