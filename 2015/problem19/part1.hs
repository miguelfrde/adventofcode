import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

type Replacement = (String, String)

parseLine :: String -> Replacement
parseLine line = case splitOn " " line of [a, "=>", b] -> (a, b)

moleculesFrom :: String -> Replacement -> [String]
moleculesFrom string (old, new)
    | not (old `isInfixOf` string) = [string]
    | otherwise = moleculesFrom' [head parts] (tail parts) []
    where parts = splitOn old string
          moleculesFrom' _ [] result = result
          moleculesFrom' previous next@(s:rest) r = moleculesFrom' (previous ++ [s]) rest (r ++ [newMolecule])
              where newMolecule = (intercalate old previous) ++ new ++ (intercalate old next)

solve :: String -> [String] -> Int
solve molecule lineList = length . nub . filter (/= molecule) .  concatMap (moleculesFrom molecule) $ replacements
    where replacements = map parseLine lineList

main = do
    [string, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve string . lines $ content
