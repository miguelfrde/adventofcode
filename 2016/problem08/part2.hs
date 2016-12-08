import System.Environment
import Data.List.Split (splitOn)
import Data.List (intercalate, transpose)

data Instruction
  = Rect Int Int
  | RotateColumn Int Int
  | RotateRow Int Int
  deriving (Show)

type Screen = [String]


initialBoard :: [String]
initialBoard = replicate 6 . replicate 50 $ '.'


apply :: Screen -> Instruction -> Screen
apply screen (Rect x y) = affectedLines ++ unaffectedLines
  where affectedLines = map (\line -> (replicate x '#') ++ (drop x line)) . take y $ screen
        unaffectedLines = drop y screen
apply screen (RotateColumn x units) = transpose .  apply (transpose screen) $ RotateRow x units
apply screen (RotateRow y units) = firstRows ++ (shiftedRow:nextRows)
  where firstRows = take y screen
        n = length row
        row = screen !! y
        nextRows = drop (y + 1) screen
        shiftedRow = take n . drop (n - units) . concat . repeat $ row


countLit :: [String] -> Int
countLit = sum . map (length . filter (=='#'))

parseInput :: String -> [Instruction]
parseInput = map (parse . words) . lines
  where parse ["rect", coords] = Rect (head coords') (coords' !! 1)
          where coords' = map read . splitOn "x" $ coords
        parse ["rotate", "row", expr, "by", units] = RotateRow (assignmentToVal expr) (read units)
        parse ["rotate", "column", expr, "by", units] = RotateColumn (assignmentToVal expr) (read units)
        assignmentToVal = read . (!! 1) . splitOn "="


solve :: String -> Screen
solve = foldl apply initialBoard . parseInput


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  mapM_ putStrLn . solve $ content
