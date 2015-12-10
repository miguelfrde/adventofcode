import System.Environment
import Data.List

hasPairWithoutOverlap :: String -> Bool
hasPairWithoutOverlap (a:bs@(b:cs))
    | [a, b] `isInfixOf` cs = True
    | otherwise = hasPairWithoutOverlap bs
hasPairWithoutOverlap _ = False

hasOneRepeatingWithOneInBetween :: String -> Bool
hasOneRepeatingWithOneInBetween (a:bs@(_:b:_))
    | a == b    = True
    | otherwise = hasOneRepeatingWithOneInBetween bs
hasOneRepeatingWithOneInBetween _ = False

isNice :: String -> Bool
isNice string = (hasPairWithoutOverlap string) &&
                (hasOneRepeatingWithOneInBetween string)

solve :: [String] -> Integer
solve strings = foldl (\acc string -> acc + (value string)) 0 strings
    where value string = if isNice string then 1 else 0

main :: IO ()
main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve . lines $ content
