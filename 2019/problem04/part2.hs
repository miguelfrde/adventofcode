import System.Environment

-- TODO
solve :: String -> Integer
solve string = 0

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
