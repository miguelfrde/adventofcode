import System.Environment

fuel :: Int -> Int
fuel x = (quot x 3) - 2

totalFuel :: Int -> Int
totalFuel = sum . takeWhile (>0) . tail . iterate fuel

solve :: String -> Int
solve string = sum . map (totalFuel . toInteger) . lines $ string
  where toInteger = read::String->Int

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
