import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5, md5DigestBytes)
import Data.List (tails, isInfixOf)
import System.Environment


getRepeatedChar :: String -> Maybe Char
getRepeatedChar [] = Nothing
getRepeatedChar (_:_:[]) = Nothing
getRepeatedChar (a:str@(b:c:cs))
  | a == b && b == c = Just a
  | otherwise = getRepeatedChar str


valid :: [(Int, String)] -> Bool
valid ((_, key):keys) = case getRepeatedChar key of
  Nothing -> False
  Just c -> any (isInfixOf (replicate 5 c) . snd) . take 1000 $ keys


makeHash :: String -> Int -> String
makeHash key n = iterate makeHash' (makeHash' (key ++ (show n))) !! 2016
  where makeHash' = show . md5 . pack


solve :: String -> Int
solve key = fst . head . last . take 64 . filter valid $ hashSets
  where hashSets = tails [(x, makeHash key x) | x <- [0..]]


main :: IO ()
main = do
  [string] <- getArgs
  putStrLn . show . solve $ string
