{-# Language RecordWildCards #-}

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.List.Split
import Data.Vector (Vector, (!), (//), toList, fromList)
import System.Environment

type Memory = Map.Map Integer Integer

data MemoryMessVm = MemoryMessVm {
    pc :: Integer,
    rb :: Integer,
    mem :: Memory,
    halt :: Bool
}

data Param = Ref Integer | Val Integer | RbRef Integer deriving (Show, Eq)

data Instruction =
  Add Param Param Param
  | Mul Param Param Param
  | Input Param
  | Output Param
  | JmpZero Param Param
  | JmpNz Param Param
  | CmpLe Param Param Param
  | CmpEq Param Param Param
  | SetRb Param
  | Halt
  deriving (Show, Eq)

data StepSideEffect =
    StepHalt MemoryMessVm
    | StepInput (Integer -> MemoryMessVm)
    | StepOutput Integer MemoryMessVm

fetch :: Memory -> Integer -> Integer
fetch mem index = Map.findWithDefault 0 index mem

save :: Memory -> Integer -> Integer -> Memory
save mem index value = Map.insert index value mem

evalParam :: MemoryMessVm -> Param -> Integer
evalParam vm@MemoryMessVm{..} (Ref x) = fetch mem x
evalParam vm@MemoryMessVm{..} (RbRef x) = fetch mem (rb + x)
evalParam vm@MemoryMessVm{..} (Val x) = x

updateMem :: MemoryMessVm -> Param -> Integer -> Memory
updateMem vm@MemoryMessVm{..} (Ref x) value = save mem x value
updateMem vm@MemoryMessVm{..} (RbRef x) value = save mem (rb + x) value
updateMem vm@MemoryMessVm{..} (Val _) _ = error "unavaiable"

currentValue :: MemoryMessVm -> Integer -> Integer
currentValue vm@MemoryMessVm{..} i = fetch mem (pc + i)

param :: Integer -> Integer -> Param
param 0 x = Ref x
param 1 x = Val x
param 2 x = RbRef x

currentInstruction :: MemoryMessVm -> Instruction
currentInstruction vm = case opCode of
  1 -> Add (param' 1) (param' 2) (param' 3)
  2 -> Mul (param' 1) (param' 2) (param' 3)
  3 -> Input (param' 1)
  4 -> Output (param' 1)
  5 -> JmpNz (param' 1) (param' 2)
  6 -> JmpZero (param' 1) (param' 2)
  7 -> CmpLe (param' 1) (param' 2) (param' 3)
  8 -> CmpEq (param' 1) (param' 2) (param' 3)
  9 -> SetRb (param' 1)
  99 -> Halt
  x -> error ("Unknown op " ++ show x)
  where opCode = val 0 `mod` 100
        getMode i x = x `div` (10^i) `mod` 10
        val = currentValue vm
        param' i = param (getMode (i+1) (val 0)) (val i)

evalOp :: MemoryMessVm -> Instruction -> MemoryMessVm
evalOp vm@MemoryMessVm{..} inst  = case inst of
    Add a b out  -> vm{ pc = pc + 4, mem = updateMem vm out (op a + op b) }
    Mul a b out  -> vm{ pc = pc + 4, mem = updateMem vm out (op a * op b) }
    JmpNz a b -> evalJmp (/=0) a b
    JmpZero a b -> evalJmp (==0) a b
    CmpLe a b out -> evalCmp (<) a b out
    CmpEq a b out -> evalCmp (==) a b out
    SetRb out -> vm { pc = pc + 2, rb = rb + (op out) }
  where
    op = evalParam vm
    evalJmp f a b
      | f . op $ a = vm { pc = op b }
      | otherwise = vm { pc = pc + 3 }
    evalCmp f a b out =
      let val = if f (op a) (op b) then 1 else 0
      in vm { pc = pc + 4, mem = updateMem vm out val }


execute :: MemoryMessVm -> StepSideEffect
execute vm@MemoryMessVm{..} = case current of
    Halt -> StepHalt vm { halt = True }
    Input out -> StepInput (\stdin -> vm { pc = pc + 2, mem = updateMem vm out stdin })
    Output o -> StepOutput (evalParam vm o) (vm { pc = pc + 2 })
    _ -> execute . evalOp vm $ current
  where current = currentInstruction vm

parseNumberList :: String -> [Integer]
parseNumberList = map toInteger . splitOn ","
  where toInteger = read::String->Integer

loadProgram :: [Integer] -> MemoryMessVm
loadProgram program = MemoryMessVm {
    pc = 0, rb = 0, mem = mem, halt = False }
  where mem = Map.fromList . zip [0..] $ program

data Direction = Up | Down | DRight | DLeft deriving (Show, Eq)
data Action = TurnDLeft | TurnDRight deriving (Show, Eq)
data Color = Black | White deriving (Show, Eq)

data State = State {
    grid :: Grid,
    position :: (Integer, Integer),
    direction :: Direction
} deriving (Show, Eq)

type Grid = Map.Map (Integer, Integer) Color

intVal :: Color -> Integer
intVal Black = 0
intVal White = 1

emptyGrid :: Grid
emptyGrid = Map.empty

color :: State -> Color
color state@State{..} = colorAt state position

colorAt :: State -> (Integer, Integer) -> Color
colorAt state@State{..} pos = Map.findWithDefault Black pos grid

displayColor :: Color -> Char
displayColor Black = '.'
displayColor White = '#'


displayState :: State -> String
displayState state@State{..} = intercalate "\n" [map (\x -> displayColor . colorAt state $ (x, y)) [minX..maxX] | y <- [minY..maxY]]
    where minX = minimum . map fst . Map.keys $ grid
          maxX = maximum . map fst . Map.keys $ grid
          minY = minimum . map snd . Map.keys $ grid
          maxY = maximum . map snd . Map.keys $ grid

move :: State -> Direction -> State
move state@State{..} dir = state{ position = (x + dx, y + dy), direction = dir }
  where  (x, y) = position
         (dx, dy) = case dir of
             DRight -> (1, 0)
             DLeft -> (-1, 0)
             Up -> (0, -1)
             Down -> (0, 1)

makeTurn :: Integer -> State -> State
makeTurn dir state@State{..} = case direction of
    Up -> move state ([DLeft, DRight] !! d)
    Down -> move state ([DRight, DLeft] !! d)
    DLeft -> move state ([Down, Up] !! d)
    DRight -> move state ([Up, Down] !! d)
  where d = fromIntegral dir

paint :: Integer -> State -> State
paint 0 state@State{..} = state{ grid = Map.insert position Black grid }
paint 1 state@State{..} = state{ grid = Map.insert position White grid }

data SideEffect =
  EffectInput (Integer -> SideEffect)
  | EffectOutput Integer SideEffect
  | EffectHalt

exec :: MemoryMessVm -> SideEffect
exec vm = case execute vm of
    StepHalt _ -> EffectHalt
    StepInput f -> EffectInput (exec . f)
    StepOutput o vm' -> EffectOutput o (exec vm')

paintRun :: State -> MemoryMessVm -> State
paintRun state vm = paintRun' state (exec vm)
  where
    paintRun' state@State{..} effect = case effect of
        EffectHalt -> state
        EffectInput f -> paintRun' state (f . intVal . color $ state)
        EffectOutput c (EffectOutput turn e) -> paintRun' (makeTurn turn . paint c $ state) e

solve :: String -> Int
solve input = length . Map.elems . grid . paintRun initialState  $ vm
  where startup = parseNumberList $ input
        vm = loadProgram startup
        initialState = State { position = (0, 0), direction = Up, grid = emptyGrid }

display :: String -> String
display input = displayState . paintRun initialState  $ vm
  where startup = parseNumberList $ input
        vm = loadProgram startup
        grid = Map.insert (0, 0) White emptyGrid
        initialState = State { position = (0, 0), direction = Up, grid = grid }

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
  putStrLn . display $ content
