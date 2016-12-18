import System.Environment
import Data.List (zip4)

data Disc = Disc {totalPositions :: Int, initialPosition :: Int} deriving (Show)


parseInput :: String -> [Disc]
parseInput = map parseLine . lines
  where parseLine line = case words line of
          [_, _, _, positions, _, _, _, _, _, _, _, curr] -> Disc (read positions :: Int) (read (init curr) :: Int)


validTimes :: [Disc] -> [Int]
validTimes discs = [t | t <- [0..], validTime t]
  where validTime t = all (\(t, p, i, x) -> (t + i + x) `mod` p == 0) (zipped t)
        initials = map initialPosition discs
        positions = map totalPositions discs
        zipped t = zip4 (replicate (length discs) t) positions initials [1..]


solve :: String -> Int
solve = head . validTimes . parseInput

solve2 :: String -> Int
solve2 = head . validTimes . (++ [Disc 11 0]) . parseInput

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve2 $ content
