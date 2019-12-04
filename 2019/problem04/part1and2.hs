import Data.List (group)
import Data.List.Split
import System.Environment

increasing :: [Char] -> Bool
increasing cs = all (== True) . zipWith (<=) cs . tail $ cs

solve :: (Int -> Bool) -> String -> Int
solve predicate input = length . filter (any predicate) . map groupLengths $ candidates
  where [a, b] = map toInt . splitOn "-" $ input
        toInt = read::String->Int
        candidates = filter increasing . map show $ [a..b]
        groupLengths = map length . group

solve1 :: String -> Int
solve1 = solve (>= 2)

solve2 :: String -> Int
solve2 = solve (== 2)

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve2 $ content
