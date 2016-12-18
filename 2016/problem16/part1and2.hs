import System.Environment
import Data.List (find)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

-- Use: 272 for part1
diskLength :: Int
diskLength = 35651584

generateData :: String -> String
generateData a = a ++ "0" ++ (map switch . reverse $ a)
  where switch '0' = '1'
        switch '1' = '0'


solve :: String -> String
solve string = fromJust . find (odd . length) . iterate transform $ initialStr
  where initialStr = take diskLength . fromJust . find (\x -> length x >= diskLength) . iterate generateData $ string
        transform = map (\(a:b:[]) -> if a == b then '1' else '0') . chunksOf 2


main :: IO ()
main = do
  [input] <- getArgs
  putStrLn . show . solve $ input
