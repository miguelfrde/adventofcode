import System.Environment
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

data Range = Range Integer Integer deriving (Eq, Ord, Show)


-- Assuming already sorted
merge :: Range -> Range -> Maybe Range
merge (Range a b) (Range c d)
  | b >= c = Just (Range a (max b d))
  | otherwise = Nothing


--Assuming already sorted
simplify :: [Range] -> [Range]
simplify (r1:rs@(r2:rest)) = case merge r1 r2 of
  Nothing -> (r1:(simplify rs))
  Just r -> simplify (r:rest)
simplify x = x


filterIps :: [Range] -> [Integer]
filterIps = filterIps' 0 []
  where filterIps' ip result [] = result ++ [ip..4294967295]
        filterIps' ip result ranges@((Range lo hi):rest)
          | ip > 4294967295 = result
          | ip < lo = filterIps' (hi + 1) (result ++ [ip..lo-1]) rest
          | ip == lo = filterIps' (ip + hi - lo + 1) result rest
          | ip >= hi = filterIps' (ip + 1) result rest


parseInput :: String -> [Range]
parseInput = map parseLine . lines
  where parseLine string = case splitOn "-" string of
            [x, y] -> Range (read x :: Integer) (read y :: Integer)


solve :: String -> Integer
solve = fromIntegral . length . filterIps . simplify . sort . parseInput
--solve = head . filterIps . simplify . sort . parseInput


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
