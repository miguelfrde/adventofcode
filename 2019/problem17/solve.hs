{-# Language RecordWildCards #-}

import Data.Maybe
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
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

data SideEffect =
  EffectInput (Integer -> SideEffect)
  | EffectOutput Integer SideEffect
  | EffectHalt

exec :: MemoryMessVm -> SideEffect
exec vm = case execute vm of
    StepHalt _ -> EffectHalt
    StepInput f -> EffectInput (exec . f)
    StepOutput o vm' -> EffectOutput o (exec vm')

bfs :: (Ord a)
  => (a -> Set.Set a)  -- get neighbors
  -> (a -> Bool) -- validate exit criteria
  -> a  -- start
  -> [a]  -- result
bfs neighborsFn valid start = bfs' [(start, [])] Set.empty
  where
    bfs' [] _ = []
    bfs' ((x, xs):queue) visited
      | valid x = reverse (x:xs)
      | Set.member x visited = bfs' queue visited
      | otherwise = bfs' (queue ++ neighborQueue) (Set.insert x visited)
      where xNeighbors = neighborsFn x
            neighborQueue = map (\n -> (n, x:xs)) . Set.elems $ xNeighbors

dfs :: (Ord a)
  => (a -> Set.Set a) -- get neighbors
  -> a  -- start
  -> Set.Set a  -- result
dfs neighborFn from = dfs' from Set.empty
  where
   dfs' curr visited
     | elem curr visited = visited
     | null nexts = Set.insert curr visited
     | otherwise = foldl (\acc n -> dfs' n acc) (Set.insert curr visited) nexts
     where nexts = neighborFn curr

data Node = Scaffold | Empty deriving (Show, Eq, Ord);

type ScaffoldMap = Map.Map (Integer, Integer) Node;
data State = State {
    coord :: ScaffoldMap,
    robot :: (Integer, Integer),
    direction :: Direction
} deriving (Show, Eq, Ord)

data Direction = Up | Down | DRight | DLeft deriving (Show, Eq, Ord)

loadMap :: MemoryMessVm -> State
loadMap vm = loadMap' newMap (0,0) (exec vm)
  where newMap = State { robot = (-1,-1), coord = Map.empty, direction = Down }
        loadMap' state@State{..} (x, y) effect = case effect of
            EffectHalt -> state
            EffectOutput 35 e -> loadMap' state{ coord = Map.insert (x, y) Scaffold coord } (x+1, y) e
            EffectOutput 46 e -> loadMap' state{ coord = Map.insert (x, y) Empty coord } (x+1, y) e
            EffectOutput 10 e -> loadMap' state (0, y + 1) e
            EffectOutput o e -> loadMap' state{ coord = Map.insert (x, y) Scaffold coord, robot = (x, y), direction = dir o } (x+1, y) e
        dir 94 = Up
        dir 118 = Down
        dir 60 = DLeft
        dir 64 = DRight

display :: State -> String
display state@State{..} = intercalate "\n" $ [[display' (x,y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where minX = minimum . map fst . Map.keys $ coord
        maxX = maximum . map fst . Map.keys $ coord
        minY = minimum . map snd . Map.keys $ coord
        maxY = maximum . map snd . Map.keys $ coord
        display' c = case coord Map.! c of
            Empty -> '.'
            Scaffold
              | robot /= c -> '#'
              | otherwise -> case direction of
                  Up -> '^'
                  Down -> 'v'
                  DLeft -> '>'
                  DRight -> '<'

isScaffold :: ScaffoldMap -> (Integer,Integer) -> Bool
isScaffold m x = Map.lookup x m == (Just Scaffold)

adjacent :: (Integer,Integer) -> [(Integer,Integer)]
adjacent (x,y) = [(x,y),(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

findIntersections :: Map.Map (Integer, Integer) Node -> Integer
findIntersections m = find' (Map.keys m) 0
  where find' [] acc = acc
        find' ((x, y):rest) acc
          | allBelong = find' rest (acc + x*y)
          | otherwise = find' rest acc
          where allBelong = and [isScaffold m x | x <- (x,y):(adjacent (x,y)) ]


findPath :: State -> [(Integer,Integer)]
findPath state@State{..} = bfs neighbors success $ robot
  where success p = p == (28,12)
        neighbors p = Set.fromList [c | c <- adjacent p, isScaffold coord c]

solve :: String -> Integer
solve input = findIntersections . coord $ state
  where startup = parseNumberList $ input
        vm = loadProgram startup
        state = loadMap vm

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
  putStrLn . display . loadMap . loadProgram . parseNumberList $ content
