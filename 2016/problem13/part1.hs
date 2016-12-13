import System.Environment
import qualified Data.Set as Set
import Data.Bits


isOpen n (x, y) = (bits number) `mod` 2 == 0
  where number = x*x + 3*x + 2*x*y + y + y*y + n
        bits 0 = 0
        bits n = 1 + bits (n .&. (n - 1))


openNeighbors n (x, y) = filter (\c -> isOpen n c && validNeighbor c) neighbors
  where neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        validNeighbor (x, y) = x >= 0 && y >= 0


countSteps start goal n = bfs' [(start, 0)] Set.empty
  where bfs' [] _ = -1  -- error, nothing else to explore
        bfs' ((current, steps):rest) explored
          | current == goal = steps
          | Set.member current explored = bfs' rest explored
          | otherwise = bfs' (rest ++ nextStates) (Set.insert current explored)
          where nextStates = map (\c -> (c, steps + 1)) . openNeighbors n $ current


solve :: Int -> Int
solve = countSteps (1, 1) (31, 39)

main :: IO ()
main = do
  [n] <- getArgs
  putStrLn . show . solve . (read :: (String -> Int)) $ n
