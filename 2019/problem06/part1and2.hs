import Data.List
import Data.List.Split
import Data.Set (Set)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Environment

type Graph a = Map a (Set a)

neighbors :: (Ord a) => Graph a -> a -> Set a
neighbors g x = case Map.lookup x g of
  Nothing -> Set.empty
  Just ns -> ns

-- Computes all reachable nodes from `from` in the graph (including `from`).
dfs :: (Ord a)
  => (a -> Set a) -- get neighbors
  -> a  -- start
  -> Set a  -- result
dfs neighborFn from = dfs' from Set.empty
  where
   dfs' curr visited
     | elem curr visited = visited
     | null nexts = Set.insert curr visited
     | otherwise = foldl (\acc n -> dfs' n acc) (Set.insert curr visited) nexts
     where nexts = neighborFn curr

bfs :: (Ord a)
  => (a -> Set a)  -- get neighbors
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

getGraph :: String -> Graph String
getGraph input = Map.fromListWith (Set.union) [(y, Set.singleton x) | [x, y] <- inps]
  where inps = map (splitOn ")") . lines $ input

indirectOrbits :: (Ord a) => Graph a -> a -> Int
indirectOrbits graph x = (subtract (direct + 1)) . length . dfs neighborFn $ x
  where neighborFn = neighbors graph
        direct = length . neighbors graph $ x
        -- subtract: `self` and the direct orbits

solve :: String -> Int
solve input = indirect + direct
  where graph = getGraph input
        indirect = sum . map (indirectOrbits graph) . Map.keys $ graph
        direct = sum . map length . Map.elems $ graph

-- Ex: Y = [YOU,B,C,D,COM], S = [SAN,Y,C,D,COM] = [B,C] = 5 + 5 - 2 * (3 + 1)
solve2 :: String -> Int
solve2 input =  length pathYou + length pathSan - 2 * (length common + 1)
  where graph = getGraph input
        neighborFn x = neighbors graph x
        valid = (== "COM")
        pathYou = bfs neighborFn valid $ "YOU"
        pathSan = bfs neighborFn valid $ "SAN"
        common = intersect pathYou pathSan

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
  putStrLn . show . solve2 $ content
