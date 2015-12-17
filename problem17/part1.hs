import System.Environment
import Data.List

solve :: Int -> [Int] -> Int
solve liters = length . filter (\combination -> sum combination == liters) . subsequences

main = do
    [liters, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve (read liters) . map read . lines $ content
