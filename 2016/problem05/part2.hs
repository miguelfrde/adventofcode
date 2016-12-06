import System.Environment
import Data.Char (digitToInt)
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8
import Data.Ord(comparing)
import qualified Data.List as List
import qualified Data.Map.Strict as Map


isProperHash :: String -> Bool
isProperHash string = (pack "00000") `isPrefixOf` (pack string)


makeHash :: String -> Int -> String
makeHash key n = show . md5 . pack $ key ++ (show n)


solve :: String -> String
solve string = buildFromMap . solve' Map.empty $ 1
  where buildFromMap = List.map snd . List.sortOn fst . Map.toList
        solve' characters n
          | Map.size characters == 8 = characters
          | index >= 8 = solve' characters (n + 1)
          | isProperHash currHash && (index `Map.notMember` characters)
              = solve' (Map.insert index character characters) (n + 1)
          | otherwise = solve' characters (n + 1)
          where currHash = makeHash string n
                index = digitToInt (currHash !! 5)
                character = currHash !! 6


main :: IO ()
main = do
  [word] <- getArgs
  Prelude.putStrLn . show . solve $ word
