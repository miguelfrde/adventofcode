import Data.List.Split
import System.Environment

surface :: Integer -> Integer -> Integer -> Integer
surface l w h = 2*l*w + 2*w*h + 2*h*l

areaSmallestSide :: Integer -> Integer -> Integer -> Integer
areaSmallestSide l w h = minimum [l*w, w*h, h*l]

solveLine :: String -> Integer
solveLine string = (surface l w h) + (areaSmallestSide l w h)
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
