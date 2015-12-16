import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

data Aunt = Aunt
    { name :: String
    , number :: Int
    , things :: [(String, Int)]
    } deriving (Show)

knownThings :: Map.Map String Int
knownThings = Map.fromList [
    ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)]

parseLine :: String -> Aunt
parseLine line = case splitOn " " line of
    [name, number, a, aQuantity, b, bQuantity, c, cQuantity] ->
        Aunt
        { name = name
        , number = value number
        , things = [(init a, value aQuantity), (init b, value bQuantity), (init c, read cQuantity)]
        }
    where value = read . init

findAunt :: [Aunt] -> Maybe Aunt
findAunt aunts = find isTheRightAunt aunts
    where isTheRightAunt = all (\(thing, value) -> knownThings Map.! thing == value) . things

solve :: [String] -> Maybe Int
solve lineList = fmap number $ findAunt aunts
    where aunts = map parseLine lineList

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve . lines $ content
