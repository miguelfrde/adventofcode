import Data.List (transpose, concatMap)
import Data.List.Split (chunksOf)
import System.Environment

type Triangle = (Integer, Integer, Integer)

listToTuple :: [Integer] -> Triangle
listToTuple (x:y:z:[]) = (x, y, z)

isTriangle :: Triangle -> Bool
isTriangle (x, y, z) = x + y > z && x + z > y && y + z > x

parseInput :: String -> [Triangle]
parseInput = map listToTuple . concatMap (chunksOf 3) . transpose . map (map read . words) . lines

solve :: String -> Int
solve = length . filter isTriangle . parseInput

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
