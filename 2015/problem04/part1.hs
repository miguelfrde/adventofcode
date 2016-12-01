import System.Environment
import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8

isProperHash :: String -> Bool
isProperHash string = (pack "00000") `isPrefixOf` (pack string)

solve' :: String -> Integer -> Integer
solve' baseKey num
    | isProperHash md5Hash = num
    | otherwise            = solve' baseKey (num + 1)
    where key = baseKey ++ (show num)
          md5Hash = show . md5 . pack $ key

solve :: String -> Integer
solve key = solve' key 0

main = do
    [word] <- getArgs
    Prelude.putStrLn . show . solve $ word
