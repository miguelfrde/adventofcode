import Data.List.Split (splitOn)
import System.Environment


solve :: String -> Int
solve [] = 0
solve ('(':cs) = n*m + solve (drop n ys)
  where (n, xs) = splitter 'x' cs
        (m, ys) = splitter ')' xs
        splitter c s = ((read :: String -> Int) . takeWhile (/=c) $ s, tail . dropWhile (/=c) $ s)
solve (_:cs) = 1 + solve cs


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . sum . map solve . lines $ content
