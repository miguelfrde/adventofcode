import System.Environment


data Direction = U | R | D | L deriving (Show, Read)
type Position = (Int, Int)

keypad :: [String]
keypad = [
  "xx1xx",
  "x234x",
  "56789",
  "xABCx",
  "xxDxx"]


step :: Position -> Direction -> Position
step pos dir = if (positionToSymbol pos') /= 'x' then pos' else pos
  where pos' = step' pos dir
        step' (0, y) L = (0, y)
        step' (x, y) L = (x - 1, y)
        step' (4, y) R = (4, y)
        step' (x, y) R = (x + 1, y)
        step' (x, 0) U = (x, 0)
        step' (x, y) U = (x, y - 1)
        step' (x, 4) D = (x, 4)
        step' (x, y) D = (x, y + 1)


parseInput :: String -> [[Direction]]
parseInput = map parseLine . lines
  where parseLine = map parseDirection
        parseDirection 'U' = U
        parseDirection 'R' = R
        parseDirection 'D' = D
        parseDirection 'L' = L


positionToSymbol :: Position -> Char
positionToSymbol (x, y) = (keypad !! y) !! x


solve :: String -> String
solve = map positionToSymbol . tail . scanl (foldl step) (0, 2) . parseInput


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
