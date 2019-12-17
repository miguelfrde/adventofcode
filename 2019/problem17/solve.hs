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

findIntersections :: Map.Map (Integer, Integer) Node -> [(Integer, Integer)]
findIntersections m = find' (Map.keys m) 0
  where find' [] acc = acc
        find' ((x, y):rest) acc
          | allBelong = find' rest (x,y):acc
          | otherwise = find' rest acc
          where allBelong = and [isScaffold m x | x <- (x,y):(adjacent (x,y)) ]

data Turn = TurnLeft | TurnRight | Forward Integer deriving (Show, Eq, Ord);

bfs :: (Ord a)
  => (a -> Set.Set a)  -- get neighbors
  -> (a -> Bool) -- validate exit criteria
  -> (a -> Bool) -- whether or not a state has been visited
  -> (a -> a) -- visit state
  -> a  -- start
  -> [a]  -- result
bfs neighborsFn valid canVisit visit start = bfs' [(start, [])] Set.empty
  where
    bfs' [] _ = []
    bfs' ((x, xs):queue) visited
      | valid x = reverse (x:xs)
      | not . canVisit $ x = bfs' queue visited
      | otherwise = bfs' (queue ++ neighborQueue)
      where xNeighbors = neighborsFn x
            x' = visit x
            neighborQueue = map (\n -> (n, x':xs)) . Set.elems $ xNeighbors

displayTurn :: Turn -> String
displayTurn TurnLeft = "L"
displayTurn TurnRight = "R"
displayTurn (Forward n) = show n

displayDirs :: [Turn] -> [String]
displayDirs = reverse . display' []
  where display' r [] = r
        display' r [t] = (displayTurn t):r
        display' r (t1:t2:ts) = display' ((displayTurn t1 ++ displayTurn t2):r) ts

asMovements :: Direction -> [(Integer,Integer)] -> [Turn]
asMovements dir path = tail . reverse . asMovements' dir path $ [(Forward 0)]
  where
    asMovements' _ [] res =  res
    asMovements' dir ((x,y):[]) res = res
    asMovements' dir ((x,y):rest@((x',y'):xs)) result@((Forward n):rs) =
      let turn turnDir newDir = asMovements' newDir rest ((Forward 1):turnDir:result)
          forward = asMovements' dir rest ((Forward (n+1)):rs)
      in case dir of
            Up -> if (y' < y) && (x == x') then forward
                  else if x' > x then turn TurnRight DRight
                  else turn TurnLeft DLeft
            Down -> if (y' > y) && (x == x') then forward
                    else if x' > x then turn TurnLeft DRight
                    else turn TurnRight DLeft
            DRight -> if (x' > x) && (y == y') then forward
                      else if y' > y then turn TurnRight Down
                      else turn TurnLeft Up
            DLeft -> if (x' < x) && (y == y') then forward
                     else if y' > y then turn TurnLeft Down
                     else turn TurnRight Up

data SearchState = SearchState {
    current :: (Integer, Integer)
    visited :: Set.Set (Integer, Integer)
    interesctionCount :: Map.Map (Integer, Integer) Int

} deriving (Show, Eq, Ord)

-- TODO: this could be more efficient
findPath :: State -> [(Integer,Integer)]
findPath state@State{..} = fromJust . bfs neighbors success canVisit vist $ initial
  where
    intersections = findIntersections . coord $ state
    initial = SearchState {
        pos = robot,
        visited = Set.empty,
        intersectionCount = Map.fromList . map (\i -> (i, 0)) $ intersections
        }

    -- Visiting a state means marking it as visited and increasing the intersection count if it's
    -- an intersection
    visit s@SearchState{..} = s{ current = }

    -- A state can be visited if: it's not an interesection and hasn't been visited,
    -- it's an intersection and has been visited at most once.
    canVisit s@SearchState{..}

    -- Success means we found the target and the path includes all nodes.
    success p = p == (28,12) && (length p) == (. length . filter (==Scaffold) . Map.elems $ coord)

    -- All neighboring scaffolds
    neighbors p = Set.fromList [c | c <- adjacent p, isScaffold coord c]

solve :: String -> Integer
solve input = sum . map (\(x, y) -> x*y) . findIntersections . coord $ state
  where startup = parseNumberList $ input
        vm = loadProgram startup
        state = loadMap vm

solve2 :: String -> [Turn]
solve2 input = asMovements (direction state) . findPath $ state
  where startup = parseNumberList $ input
        vm = loadProgram startup
        state = loadMap vm
        --state = loadMap vm{mem = save mem 0 2}

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  --putStrLn . show . solve $ content
  putStrLn . display . loadMap . loadProgram . parseNumberList $ content
  putStrLn . show . displayDirs . solve2 $ content
