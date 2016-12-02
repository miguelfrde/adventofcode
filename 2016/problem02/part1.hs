import System.Environment


data Direction = U | R | D | L deriving (Show, Read)
type Position = (Int, Int)


step :: Position -> Direction -> Position
step (0, y) L = (0, y)
step (x, y) L = (x - 1, y)
step (2, y) R = (2, y)
step (x, y) R = (x + 1, y)
step (x, 0) U = (x, 0)
step (x, y) U = (x, y - 1)
step (x, 2) D = (x, 2)
step (x, y) D = (x, y + 1)


parseInput :: String -> [[Direction]]
parseInput = map parseLine . lines
  where parseLine = map parseDirection
        parseDirection 'U' = U
        parseDirection 'R' = R
        parseDirection 'D' = D
        parseDirection 'L' = L


solve :: String -> String
solve = map positionToSymbol . tail . scanl (foldl step) (1, 1) . parseInput
  where positionToSymbol (x, y) = (keypad !! y) !! x
        keypad = [['1'..'3'], ['4'..'6'], ['7'..'9']]


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
