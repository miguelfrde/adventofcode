import System.Environment

fuel :: Int -> Int
fuel x = (quot x 3) - 2

solve :: String -> Int
solve string = sum . map (fuel . toInteger) . lines $ string
  where toInteger = read::String->Int

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
