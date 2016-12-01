import System.Environment
import Data.List.Split
import qualified Data.Map.Strict as Map

type Grid = Map.Map (Int, Int) Char

corners :: (Int, Int) -> [(Int, Int)]
corners (maxX, maxY) = [(0, 0), (0, maxY), (maxX, 0), (maxX, maxY)]

adjacentValues :: Grid -> (Int, Int) -> [Char]
adjacentValues grid (x, y) = map (\coord -> grid Map.! coord) adjacentCoords
    where adjacentCoords = [ (i, j)
                           | i <- [x - 1..x + 1]
                           , j <- [y - 1..y + 1]
                           , (i, j) /= (x, y) && Map.member (i, j) grid]

nextState :: (Int, Int) -> Grid -> Grid
nextState bounds grid = Map.fromList . map (\c -> (c, nextState' c)) $ Map.keys grid
    where nextState' coord
            | isCorner = '#'
            | value == '#' && lightsOn /= 2 && lightsOn /= 3 = '.'
            | value == '.' && lightsOn == 3 = '#'
            | otherwise = value
            where lightsOn = length . filter (== '#') . adjacentValues grid $ coord
                  value = grid Map.! coord
                  isCorner = coord `elem` (corners bounds)

makeGrid :: String -> Grid
makeGrid string = makeMap Map.empty 0 grid
    where grid = [line | line <- lines string]
          makeMap m y (cs:rest) = makeMap (insertInMap m cs y) (y + 1) rest
          makeMap m _ [] = m
          insertInMap m items y = foldl (\m (x, c) -> Map.insert (x, y) c m) m $ zip [0..] items

countLightsOn :: Grid -> Int
countLightsOn grid = length . filter (\coord -> grid Map.! coord == '#') $ Map.keys grid

solve :: Int -> String -> Int
solve iters s = countLightsOn . (!! iters) . iterate (nextState bounds) $ initialGrid'
    where initialGrid = makeGrid s
          initialGrid' = foldl (\m coord -> Map.insert coord '#' m) (makeGrid s) (corners bounds)
          bounds = (maximum . map fst $ Map.keys initialGrid, maximum . map snd $ Map.keys initialGrid)

main = do
    [iters, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve (read iters) $ content
