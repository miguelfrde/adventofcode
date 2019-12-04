import Data.List (sort)
import Data.List.Split
import Data.Map (elems, fromListWith)
import System.Environment

pairs :: [Int] -> [(Int, Int)]
pairs x = zip x . tail $ x

digits :: Int -> [Int]
digits = reverse . digits'
  where digits' x
          | x < 10 = [x]
          | otherwise = (x `mod` 10):(digits' (x `div` 10))

isSorted :: [Int] -> Bool
isSorted x = (== x) . sort $ x

hasPairs :: [Int] -> Bool
hasPairs = any (== True) . map (\(a, b) -> a == b) . pairs

hasOnePairs :: [Int] -> Bool
hasOnePairs x = any (==1) occurences
  where existingPairs = filter (\(a, b) -> a == b ) . pairs $ x
        occurences = elems . fromListWith (+) $ [(x, 1) | x <- existingPairs]

solve :: [[Int] -> Bool] -> String -> Int
solve conditions input = length  . filter isValid . map digits $ [a..b]
  where [a, b] = map read . splitOn "-" $ input
        isValid x = all (== True) . map (\f -> f x) $ conditions

solve1 :: String -> Int
solve1 = solve [hasPairs, isSorted]

solve2 :: String -> Int
solve2 = solve [hasOnePairs, isSorted]

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve2 $ content
