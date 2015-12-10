import System.Environment

value :: Char -> Integer
value '(' = 1
value ')' = -1
value x = 0

solve :: String -> Integer
solve string = foldl (\acc c -> acc + value c) 0 string

main :: IO ()
main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve $ content
