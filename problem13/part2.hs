import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

type PeopleMap = Map.Map (String, String) Integer

happiness :: PeopleMap -> [String] -> Integer
happiness peopleMap combination = happiness' ((last combination):combination)
    where happiness' (a:bs@(b:_)) = (peopleMap Map.! (a, b)) + (peopleMap Map.! (b, a)) + happiness' bs
          happiness' _ = 0

addToMap :: PeopleMap -> (String, String, Integer) -> PeopleMap
addToMap peopleMap (a, b, h) = Map.insert (a, b) h peopleMap

parseLine :: String -> (String, String, Integer)
parseLine line = case splitOn " " line of
    [a, _, "gain", h, _, _, _, _, _, _, b] -> (a, (init b), read h)
    [a, _, "lose", h, _, _, _, _, _, _, b] -> (a, (init b), -(read h))

solve :: [String] -> Integer
solve lineList = maximum . map (happiness peopleMapWithMe) $ allPossibilities
    where peopleMap = foldl (\m line -> addToMap m . parseLine $ line) Map.empty lineList
          people = nub [a | (a, _) <- Map.keys peopleMap]
          peopleMapWithMe = foldl (\m p -> addToMap (addToMap m ("Me", p, 0)) (p, "Me", 0)) peopleMap people
          allPossibilities = permutations ("Me":people)

main :: IO ()
main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve . lines $ content
