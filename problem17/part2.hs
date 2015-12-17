import System.Environment
import Data.List

solve :: Int -> [Int] -> Int
solve liters containers = length . filter (\combLength -> combLength == minimumLength) $ combinationLengths
    where combinations = filter (\combination -> sum combination == liters) . subsequences $ containers
          combinationLengths = map length $ combinations
          minimumLength = minimum combinationLengths

main = do
    [liters, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve (read liters) . map read . lines $ content
