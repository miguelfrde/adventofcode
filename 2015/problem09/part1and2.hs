import System.Environment
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type CityMap = Map.Map String [(String, Int)]

data Link = Link String (String, Int) deriving (Show)

parseLine :: String -> Link
parseLine line = case (splitOn " " line) of
    [from, "to", to, "=", cost] -> Link from (to, (read cost))


addLinkToMap :: CityMap -> Link -> CityMap
addLinkToMap cityMap (Link from (to, cost)) = insert (insert cityMap (from, to, cost)) (to, from, cost)
    where insert m (x, y, c)
              | Map.member x m = Map.insert x ((y, c):(m Map.! x)) m
              | otherwise = Map.insert x [(y, c)] m


fDistance :: CityMap -> ([Int] -> Int) -> String -> Int
fDistance cityMap f from = distance' from nodesToExplore
    where nodesToExplore = Set.delete from . Set.fromList . Map.keys $ cityMap
          distance' source toExplore
              | Set.null toExplore = 0
              | otherwise = f . map (\(to, cost) -> cost + (distance' to (Set.delete to toExplore))) $ neighbors
              where neighbors = filter (\(city, _) -> Set.member city toExplore) (cityMap Map.! source)


solve :: String -> [String] -> Int
solve minOrMax lineList = f . map (fDistance cityMap f) $ cities
    where cityMap = foldl addToMap Map.empty lineList
          addToMap m line = addLinkToMap m . parseLine $ line
          cities = Map.keys cityMap
          f = if minOrMax == "min" then minimum else maximum

main = do
    [minOrMax, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve minOrMax . lines $ content
