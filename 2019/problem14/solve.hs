import Data.List.Split
import qualified Data.Map as Map
import System.Environment


type Chemical = String
type Reactions = Map.Map Chemical (Int, [(Chemical, Int)])
type Extra = Map.Map Chemical Int

produce :: Chemical -> Int -> Reactions
produce c n = Map.empty

parse :: String -> (Chemical, (Int, [(Chemical, Int)]))
parse input = parse' parts []
    -- TODO: use a parse combinator
    where parts = map (filter (/=',')) . splitOn " " $ input
          parse' [n, c, "=>", n2, c2] acc = (c2, (read n2, (c, read n):acc))
          parse' (n:c:rest) acc = parse' rest ((c, read n):acc)
          parse' a b = error ("unexpected " ++ (show a) ++ " " ++ (show b))

useExtra :: Chemical -> Int -> Map.Map Chemical Int -> (Int, Map.Map Chemical Int)
useExtra x need extra = case Map.lookup x extra of
  Nothing -> (need, extra)
  Just extraX -> if need < extraX then (0, Map.insert x (extraX - need) extra) -- use some
                 else (need - extraX, Map.delete x extra)  -- consume it all

findOre :: String -> Int -> Reactions -> Int
findOre target quantity reactions = fst . generate target quantity $ (0, Map.empty)
  where
    generate x q (oreCount, extra)
      | needed == 0 = (oreCount, extra')
      | amount*times - needed  == 0 = result
      | amount*times - needed > 0 = (fst result, Map.insert x (amount*times - needed) (snd result))
      where (amount, factors) = reactions Map.! x
            (needed, extra') = useExtra x q extra
            times = (needed + amount - 1) `div` amount
            result = runReaction (map (\(f, n) -> (f,n*times)) factors) $ (oreCount, extra')

    runReaction :: [(Chemical, Int)] -> (Int, Extra) -> (Int, Extra)
    runReaction [] (ores, extra) = (ores, extra)
    runReaction (("ORE", n):rest) (ores, extra) = runReaction rest (n + ores, extra)
    runReaction ((c, n):rest) extra = runReaction rest (generate c n extra)

binarySearch :: (Int -> Ordering) -> Int -> Int -> Int
binarySearch f lo hi
  | mid == lo = mid
  | otherwise = case f mid of
      LT -> binarySearch f mid hi
      EQ -> mid
      GT -> binarySearch f lo mid
  where mid = (lo + hi) `div` 2

solve :: String -> Int
solve input = findOre "FUEL" 1 reactions
  where reactions = Map.fromList . map parse . lines $ input

solve2 :: String -> Int
solve2 input = binarySearch (\x -> compare (findOre "FUEL" x reactions) ore) 1 ore
  where reactions = Map.fromList . map parse . lines $ input
        ore = 1000000000000

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
  putStrLn . show . solve2 $ content
