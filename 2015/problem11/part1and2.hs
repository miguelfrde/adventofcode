import System.Environment
import Data.List
import Data.Char (ord, chr)

incrementPassword :: String -> String
incrementPassword "" = "a"
incrementPassword s
    | last s == 'z' = incrementPassword (init s) ++ "a"
    | otherwise = (init s) ++ [incChar (last s)]
    where incChar 'z' = 'a'
          incChar c = chr . (+1) . ord $ c

hasThreeIncreasingLetters :: String -> Bool
hasThreeIncreasingLetters (a:bs@(b:c:_))
    | threeEqual = True
    | otherwise  = hasThreeIncreasingLetters bs
    where threeEqual = (ord a) + 1 == (ord b) && (ord b) + 1== (ord c)
hasThreeIncreasingLetters _ = False

doesNotContainIOL :: String -> Bool
doesNotContainIOL s = all (\c -> c `notElem` s) "iol"

hasTwoDistinctPairs s = (>= 2) . length . filter hasPair $ ['a'..'z']
    where hasPair x = [x, x] `isInfixOf` s

isValidPassword :: String -> Bool
isValidPassword password = all (\condition -> condition password) conditions
    where conditions = [hasThreeIncreasingLetters, doesNotContainIOL, hasTwoDistinctPairs]

solve :: String -> Maybe String
solve = find isValidPassword . iterate incrementPassword . incrementPassword

main = do
    [password] <- getArgs
    putStrLn . show . solve $ password
