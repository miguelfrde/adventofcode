import System.Environment

value :: Char -> Integer
value '(' = 1
value ')' = -1
value x = 0

solve :: String -> Integer
solve string = solve' string 0 1

solve' :: String -> Integer -> Integer -> Integer
solve' (c:cs) floor index
    | floor' == -1 = index
    | otherwise       = solve' cs floor' index+1
    where val = value c
          floor' = floor + val
solve' [] _ index = index

main :: IO ()
main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve $ content
