import System.Environment


solve :: Int -> Int
solve n = solve' [1..n] []
  where solve' [] [] = error "No solution"
        solve' [x] [] = x
        solve' (x:_:xs) nextRound = solve' xs (x:nextRound)
        solve' xs nextRound = solve' (xs ++ reverse nextRound) []


main :: IO ()
main = do
  [n] <- getArgs
  putStrLn . show . solve $ (read n :: Int)
