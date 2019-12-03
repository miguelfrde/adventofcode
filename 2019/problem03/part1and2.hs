import Data.List.Split
import qualified Data.Map as Map
import System.Environment

type Direction = Char
type Point = (Int, Int)

center :: Point
center = (0, 0)

manhattan :: Point -> Point -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

advance :: Point -> Direction -> Point
advance (x, y) dir = case dir of
    'R' -> (x + 1, y)
    'L' -> (x - 1, y)
    'U' -> (x, y + 1)
    'D' -> (x, y - 1)

points :: Point -> [Direction] -> [(Point, Int)]
points center = tail . scanl step1 (center, 0)
  where step1 (p, s) d = (advance p d, s + 1)

-- Could be optimized to check intersection of lines rather
-- than keeping track of all points.
intersections :: Point -> [Direction] -> [Direction] -> Map.Map Point Int
intersections center a b = Map.intersectionWith (+) (pointMap a) (pointMap b)
  where pointMap = Map.fromList . points center

readDirection :: String -> [Direction]
readDirection (c:n) = replicate (read n) c

readDirections :: String -> [Direction]
readDirections = concat . map readDirection . splitOn ","

solve' :: (Map.Map Point Int -> [Int]) -> String -> Int
solve' f s = minimum . f $ xs
  where (a:b:_) = lines s
        xs = intersections center (readDirections a) (readDirections b)

solve :: String -> Int
solve = solve' $ map (manhattan center) . Map.keys

solve2 :: String -> Int
solve2 = solve' $ Map.elems

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
