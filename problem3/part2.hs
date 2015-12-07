import System.Environment
import Data.List

move :: (Integer, Integer) -> Char -> (Integer, Integer)
move (x, y) '^' = (x, y + 1)
move (x, y) '>' = (x + 1, y)
move (x, y) 'v' = (x, y - 1)
move (x, y) '<' = (x - 1, y)
move (x, y) _ = (x, y)

housesVisited :: String -> [(Integer, Integer)]
housesVisited moves = nub . scanl move (0, 0) $ moves

solve :: String -> Int
solve moves = length . nub $ houses
    where santaMoves  = [move | (index, move) <- zip [0..] moves, index `mod` 2 == 0]
          robotMoves  = [move | (index, move) <- zip [0..] moves, index `mod` 2 /= 0]
          houses = (housesVisited santaMoves) ++ (housesVisited robotMoves)

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve $ content
