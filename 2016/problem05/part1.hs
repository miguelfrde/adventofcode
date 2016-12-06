import System.Environment
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8
import qualified Data.List as List

isProperHash :: String -> Bool
isProperHash string = (pack "00000") `isPrefixOf` (pack string)


makeHash :: String -> Integer -> String
makeHash key n = show . md5 . pack $ key ++ (show n)


solve :: String -> String
solve string = List.take 8 . List.map (!! 5) $ properHashes
  where properHashes = List.filter isProperHash . List.map (makeHash string) $ [1..]


main :: IO ()
main = do
  [word] <- getArgs
  Prelude.putStrLn . show . solve $ word
