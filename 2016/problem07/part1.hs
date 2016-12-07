import Data.List
import System.Environment

data AddressBlock = Supernet String | Hypernet String deriving (Show)
data Address = Address [AddressBlock] deriving (Show)


hasAbba :: String -> Bool
hasAbba (a:xs@(b:b':a':_))
  | a == a' && b == b' && a /= b = True
  | otherwise = hasAbba xs
hasAbba _ = False


supernet :: AddressBlock -> Bool
supernet (Supernet _) = True
supernet _ = False


hypernet :: AddressBlock -> Bool
hypernet (Hypernet _) = True
hypernet _ = False


supportsTLS :: Address -> Bool
supportsTLS (Address blocks) =
    (any hasAbba' . filter supernet $ blocks) && (all (not . hasAbba') . filter hypernet $ blocks)
  where hasAbba' (Supernet s) = hasAbba s
        hasAbba' (Hypernet s) = hasAbba s


strToIp :: String -> Address
strToIp string = Address (getBlocks [] string)
  where getBlocks blocks [] = blocks
        getBlocks blocks ('[':cs) =
          case break (== ']') cs of
            (h, (_:rest))-> getBlocks ((Hypernet h):blocks) rest
        getBlocks blocks cs =
          case break (== '[') cs of
            (s, rest) -> getBlocks ((Supernet s):blocks) rest


solve :: String -> Int
solve = length . filter supportsTLS . map strToIp . lines


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
