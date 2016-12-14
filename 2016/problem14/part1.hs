import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import Data.List (tails, isInfixOf)
import System.Environment


getRepeatedChar :: String -> Int -> Maybe Char
getRepeatedChar [] _ = Nothing
getRepeatedChar str@(a:cs) n
  | length str >= n && (all (==a) . take (n-1) $ cs) = Just a
  | otherwise = getRepeatedChar cs n


valid :: [(Int, String)] -> Bool
valid ((_, key):keys) = case getRepeatedChar key 3 of
  Nothing -> False
  Just c -> any (isInfixOf (replicate 5 c) . snd) . take 1000 $ keys


makeHash :: String -> Int -> String
makeHash key n = show . md5 . pack $ key ++ (show n)


solve :: String -> Int
solve key = fst . head . last . take 64 . filter valid $ hashSets
  where hashSets = tails [(x, makeHash key x) | x <- [0..]]


main :: IO ()
main = do
  [string] <- getArgs
  putStrLn . show . solve $ string
