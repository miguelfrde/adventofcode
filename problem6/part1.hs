import System.Environment
import Data.List.Split
import qualified Data.Set as Set

type Point = (Int, Int)
type Grid = Set.Set Point
type Rectangle = (Point, Point)

data Instruction
    = On Rectangle
    | Off Rectangle
    | Toggle Rectangle
    deriving (Show)

initialGrid :: Set.Set Point
initialGrid = Set.empty

pointsRect :: Rectangle -> [Point]
pointsRect ((x, y), (x', y')) = [(xx, yy) | xx <- xRange, yy <- yRange]
    where xRange = [(min x x')..(max x x')]
          yRange = [(min y y')..(max y y')]

parsePoint :: String -> Point
parsePoint point = (read (coords !! 0), read (coords !! 1))
    where coords = splitOn "," point

parseRectangle :: String -> String -> Rectangle
parseRectangle p1 p2 = (parsePoint p1, parsePoint p2)

parseInstruction :: [String] -> Instruction
parseInstruction (t1:tokens)
    | t1 == "toggle" = makeInstruction $ tokens
    | t1 == "turn"   = makeInstruction tokens
    where makeInstruction syms
            | head syms == "on"  = On (parseRectangle (syms !! 1) (syms !! 3))
            | head syms == "off" = Off (parseRectangle (syms !! 1) (syms !! 3))
            | otherwise          = Toggle (parseRectangle (syms !! 0) (syms !! 2))

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction grid (On rect) = foldl (\acc p -> Set.insert p acc) grid (pointsRect rect)
applyInstruction grid (Off rect) = foldl (\acc p -> Set.delete p acc) grid (pointsRect rect)
applyInstruction grid (Toggle rect) = foldl insertOrDelete grid (pointsRect rect)
    where insertOrDelete grid p = if p `Set.member` grid then Set.delete p grid else Set.insert p grid

solve :: [String] -> Int
solve lineList = Set.size result
    where tokens line = splitOn " " line
          interpret grid line = applyInstruction grid (parseInstruction line)
          result = foldl interpret initialGrid (map tokens lineList)

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve . lines $ content
