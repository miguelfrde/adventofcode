import System.Environment
import Data.List

move :: (Integer, Integer) -> Char -> (Integer, Integer)
move (x, y) '^' = (x, y + 1)
move (x, y) '>' = (x + 1, y)
move (x, y) 'v' = (x, y - 1)
move (x, y) '<' = (x - 1, y)
move (x, y) _ = (x, y)

solve :: String -> Int
solve moves = length . nub . scanl move (0, 0) $ moves

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve $ content
