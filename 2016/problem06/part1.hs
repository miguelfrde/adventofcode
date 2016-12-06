import System.Environment
import Data.List
import Data.Ord


solve :: String -> String
solve = map (head . maximumBy (comparing length) . group . sort) . transpose . lines


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
