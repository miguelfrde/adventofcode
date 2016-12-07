import Data.List
import System.Environment


data AddressBlock = Supernet String | Hypernet String deriving (Show)
data Address = Address [AddressBlock] deriving (Show)


addressBlockString :: AddressBlock -> String
addressBlockString (Hypernet s) = s
addressBlockString (Supernet s) = s


supernet :: AddressBlock -> Bool
supernet (Supernet _) = True
supernet _ = False


hypernet :: AddressBlock -> Bool
hypernet (Hypernet _) = True
hypernet _ = False


abas :: String -> [String]
abas = abas' []
  where abas' result [] = result
        abas' result (a:cs@(b:a':_)) =
          if a == a' && a /= b then abas' ([a,b,a']:result) cs
          else abas' result cs
        abas' result _ = result


supportsSSL :: Address -> Bool
supportsSSL (Address blocks) = (> 0) . length $ abas' `intersect` babs'
  where abas' = concatMap abas . map addressBlockString . filter supernet $ blocks
        babs' = concatMap expectedBabs . map addressBlockString . filter hypernet $ blocks
        expectedBabs = map (\[a,b,_] -> [b,a,b]) . abas


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
solve = length . filter supportsSSL . map strToIp . lines


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
