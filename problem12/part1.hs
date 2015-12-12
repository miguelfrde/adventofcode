import System.Environment
import Data.Char (isDigit)

solve :: String -> Int
solve s = solve' s 0
    where solve' ('-':string@(c:cs)) acc
            | isDigit c = solve' (dropWhile isDigit cs) (acc - getNumber string)
            | otherwise = solve' string acc
          solve' string@(c:cs) acc
            | isDigit c = solve' (dropWhile isDigit cs) (acc + getNumber string)
            | otherwise = solve' cs acc
          solve' _ acc = acc
          getNumber string = read . takeWhile isDigit $ string

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve $ content
