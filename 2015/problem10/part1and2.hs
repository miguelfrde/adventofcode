import System.Environment

solveSingle :: String -> String
solveSingle (curr:cs) = show (1 + length same) ++ curr:"" ++ solveSingle rest
    where same = takeWhile (\c -> c == curr) cs
          rest = dropWhile (\c -> c == curr) cs
solveSingle "" = ""

solve :: Int -> String -> Int
solve times numSeq = length . (!! times) . iterate solveSingle $ numSeq

main = do
    [times, numberSequence] <- getArgs
    putStrLn . show . solve (read times) $ numberSequence
