import Data.List.Split
import System.Environment

type Position = (Integer, Integer)
data Direction = North | West | South | East deriving (Show)
data Rotation = R | L | N deriving (Eq, Read, Show)
data Action = Action Rotation Integer deriving (Show)


rotate :: Rotation -> Direction -> Direction
rotate N direction = direction
rotate rotation North = if rotation == R then West else East
rotate rotation South = if rotation == R then East else West
rotate rotation East = if rotation == R then North else South
rotate rotation West = if rotation == R then South else North


step :: Position -> Direction -> Integer -> Position
step (x, y) North steps = (x, y + steps)
step (x, y) South steps = (x, y - steps)
step (x, y) West steps = (x - steps, y)
step (x, y) East steps = (x + steps, y)


parseAction :: String -> Action
parseAction token = Action rotation steps
  where rotation = case (head token) of
          'R' -> R
          'L' -> L
          _ -> N
        steps = read (tail token) :: Integer


parseActions :: String -> [Action]
parseActions = map parseAction . splitOn ", "


distance :: Position -> Position -> Integer
distance (x, y) (x', y') = abs (x - x') + abs (y - y')


solve :: String -> Integer
solve = distance (0, 0) . solve' (0, 0) North . parseActions
  where solve' position _ [] = position
        solve' position direction ((Action rotation steps):actions) = solve' position' direction' actions
          where direction' = rotate rotation direction
                position' = step position direction' steps


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
