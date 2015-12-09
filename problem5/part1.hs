import System.Environment
import Data.List

hasAtLeastThreeVowels :: String -> Bool
hasAtLeastThreeVowels string = vowelsInString >= 3
    where vowels = filter (\c -> c `elem` "aeiou") string
          vowelsInString = length vowels

oneCharAppearsTwice :: String -> Bool
oneCharAppearsTwice (a:bs@(b:_))
    | a == b    = True
    | otherwise = oneCharAppearsTwice bs
oneCharAppearsTwice _ = False

doesNotContainSomePairs :: String -> Bool
doesNotContainSomePairs string = not (("ab" `isInfixOf` string) ||
                                      ("cd" `isInfixOf` string) ||
                                      ("pq" `isInfixOf` string) ||
                                      ("xy" `isInfixOf` string))

isNice :: String -> Bool
isNice string = (hasAtLeastThreeVowels string) &&
                (oneCharAppearsTwice string) &&
                (doesNotContainSomePairs string)

solve :: [String] -> Integer
solve strings = foldl (\acc string -> acc + (value string)) 0 strings
    where value string = if isNice string then 1 else 0

main :: IO ()
main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve . lines $ content
