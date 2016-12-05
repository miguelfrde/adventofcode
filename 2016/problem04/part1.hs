import Data.Char (isLetter, isNumber, ord)
import Data.List (sortBy, sort, group)
import Data.List.Split (splitOn)
import Data.String.Utils (startswith, replace)
import System.Environment


data EncryptedName = EncryptedName
    { encryptedName :: [String]
    , sectorId :: Integer
    , checksum :: String
    } deriving (Show)


strToEncryptedName str = EncryptedName strs sid csum
  where tokens = splitOn "-" str
        strs = filter (isLetter . head) tokens
        sid = read (takeWhile isNumber . head . filter (isNumber . head) $ tokens) :: Integer
        csum = replace "]" "" . last . splitOn "[" $ str


sorter :: (Char, Int) -> (Char, Int) -> Ordering
sorter (char1, length1) (char2, length2)
  | length1 < length2 = GT
  | length1 > length2 = LT
  | length1 == length2 = compare char1 char2


isValid :: EncryptedName -> Bool
isValid (EncryptedName strs _ csum) =
    startswith csum . map fst . sortBy sorter $ groupedStrings
  where groupedStrings = map (\s -> (head s, length s)) . group . sort . concat $ strs


parseInput :: String -> [EncryptedName]
parseInput = map strToEncryptedName . lines


solve :: String -> Integer
solve = sum . map sectorId . filter isValid . parseInput


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
