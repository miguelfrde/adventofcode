import System.Environment
import Data.List.Split (chunksOf)
import Data.List (tails)

data Tile = Trap | Safe deriving (Eq, Show)
type Row = [Tile]


nextTile :: [Tile] -> Tile
nextTile [Trap, Trap, Safe] = Trap
nextTile [Safe, Trap, Trap] = Trap
nextTile [Trap, Safe, Safe] = Trap
nextTile [Safe, Safe, Trap] = Trap
nextTile _ = Safe


generateRow :: Row -> Row
generateRow row = map nextTile triples
  where row' = (Safe:row) ++ [Safe]
        triples = take (length row) . map (take 3) . tails $ row'


parseInput :: String -> Row
parseInput = map parseChar
  where parseChar '.' = Safe
        parseChar '^' = Trap


countSafeTiles :: Row -> Int
countSafeTiles = length . filter (==Safe)


solve :: Int -> String -> Int
solve n = sum . map countSafeTiles . take n . iterate generateRow . parseInput


main :: IO ()
main = do
  [n, string] <- getArgs
  putStrLn . show . solve (read n :: Int) $ string
