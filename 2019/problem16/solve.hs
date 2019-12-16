import Data.Char(digitToInt, isSpace)
import System.Environment

strip :: String -> String
strip = f . f
   where f = reverse . dropWhile isSpace

loadSignal :: String -> [Int]
loadSignal = map digitToInt . strip

pattern :: Int -> [Int]
pattern i = drop 1 . cycle $ concat . map (replicate i) $ [0,1,0,-1]

next :: [Int] -> [Int]
next signal = [apply (pattern (i+1)) signal | i <- [0..length signal]]
  where apply pattern signal = (abs . sum . map (\(p, s) -> p*s) . zip pattern $ signal) `mod` 10

evolve :: Int -> [Int] -> [Int]
evolve phases signal = evolve' 0 signal
  where evolve' x signal
          | x == phases = signal
          | otherwise = evolve' (x+1) signal'
          where signal' = next signal

number :: [Int] -> Int
number = number' 0 0 . reverse
  where number' i acc [] = acc
        number' i acc (x:xs) = number' (i+1) (acc + x*10^i) xs

solve :: String -> String
solve = concat . map show . take 8 . evolve 100 . loadSignal

-- TODO
solve2 :: String -> String
solve2 input = concat . map show . take 8 . drop skip $ result
  where result = evolve 100 . concat . replicate 10000 . loadSignal $ input
        skip = number . take 7 $ result

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . solve $ "80871224585914546619083218645595"
  putStrLn . solve $ "19617804207202209144916044189917"
  putStrLn . solve $ "69317163492948606335995924319873"

  putStrLn . solve2 $ "80871224585914546619083218645595"
  putStrLn . solve2 $ "19617804207202209144916044189917"
  putStrLn . solve2 $ "69317163492948606335995924319873"

  putStrLn . solve $ content
  putStrLn . solve2 $ content
