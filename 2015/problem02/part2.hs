import Data.List
import Data.List.Split
import System.Environment

volume :: Integer -> Integer -> Integer -> Integer
volume l w h = l * w * h

smallestPerimeter :: Integer -> Integer -> Integer -> Integer
smallestPerimeter l w h = 2*smallOne + 2*smallTwo
    where ordered = sort [l, w, h]
          smallOne = ordered !! 0
          smallTwo = ordered !! 1

solveLine :: String -> Integer
solveLine string = (smallestPerimeter l w h) + (volume l w h)
    where strNums = splitOn "x" string
          l = read (strNums !! 0) :: Integer
          w = read (strNums !! 1) :: Integer
          h = read (strNums !! 2) :: Integer

solve :: String -> Integer
solve input = foldl (\acc line -> acc + solveLine line) 0 allLines
    where allLines = lines input

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve $ content
