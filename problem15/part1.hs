import System.Environment
import Data.List
import Data.List.Split

data Ingredient = Ingredient
    { name :: String
    , capacity :: Int
    , durability :: Int
    , flavor :: Int
    , texture :: Int
    , calories :: Int
    } deriving (Show)

maxTeaspoons = 100

parseLine :: String -> Ingredient
parseLine s = case splitOn " " s of
    [name, _, capacity, _, durability, _, flavor, _, texture, _, calories] ->
        Ingredient { name = init name
                   , capacity = withoutTrailingChar capacity
                   , durability = withoutTrailingChar durability
                   , flavor = withoutTrailingChar flavor
                   , texture = withoutTrailingChar texture
                   , calories = read calories
                   }
    where withoutTrailingChar = read . init

multiplyIngredient :: Ingredient -> Int -> [Int]
multiplyIngredient ing x = map (\prop -> x * (prop ing)) properties
    where properties = [capacity, durability, flavor, texture]

applyMix :: [Ingredient] -> [Int] -> Int
applyMix ingredients mixValues = product . map (max 0 . sum) . transpose $ ingredients'
    where ingredients' = map (\(ing, x) -> multiplyIngredient ing x) $ zip ingredients mixValues

possibleMixes :: Int -> Int -> [[Int]]
possibleMixes _ 0 = [[]]
possibleMixes nsum 1 = [[nsum]]
possibleMixes nsum items = [n:ns | n <- [0..nsum], ns <- possibleMixes (nsum - n) (items - 1)]

solve :: [String] -> Int
solve lineList = maximum . map (applyMix ingredients) $ possibleMixes maxTeaspoons (length ingredients)
    where ingredients = map parseLine lineList

main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve . lines $ content
