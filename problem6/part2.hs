import System.Environment
import Data.List.Split
import qualified Data.Map as Map

type Point = (Int, Int)
type Rectangle = (Point, Point)

data Instruction
    = On Rectangle
    | Off Rectangle
    | Toggle Rectangle
    deriving (Show)

rectContains :: Rectangle -> Point -> Bool
rectContains ((x, y), (x', y')) (xx, yy)
    = (min x x') <= xx && xx <= (max x x') && (min y y') <= yy && yy <= (max y y')

parsePoint :: String -> Point
parsePoint point = (read (coords !! 0), read (coords !! 1))
    where coords = splitOn "," point

parseRectangle :: String -> String -> Rectangle
parseRectangle p1 p2 = (parsePoint p1, parsePoint p2)

parseInstruction :: [String] -> Instruction
parseInstruction (t1:tokens)
    | t1 == "toggle" = makeInstruction tokens
    | t1 == "turn"   = makeInstruction tokens
    where makeInstruction syms
            | head syms == "on"  = On (parseRectangle (syms !! 1) (syms !! 3))
            | head syms == "off" = Off (parseRectangle (syms !! 1) (syms !! 3))
            | otherwise          = Toggle (parseRectangle (syms !! 0) (syms !! 2))

applyInstructions :: Integer -> Point -> [Instruction] -> Integer
applyInstructions value _ [] = value
applyInstructions value point (On rect:instrs)
    | rectContains rect point = applyInstructions (value + 1) point instrs
    | otherwise               = applyInstructions value point instrs
applyInstructions value point (Off rect:instrs)
    | rectContains rect point = applyInstructions (max 0 (value - 1)) point instrs
    | otherwise               = applyInstructions value point instrs
applyInstructions value point (Toggle rect:instrs)
    | rectContains rect point = applyInstructions (value + 2) point instrs
    | otherwise               = applyInstructions value point instrs

solve :: [String] -> Integer
solve lineList = sum resultGrid
    where tokens line = splitOn " " line
          instructions = map (parseInstruction . tokens) lineList
          resultGrid = [applyInstructions 0 (x, y) instructions | x <- [0..999], y <- [0..999]]

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve . lines $ content
