import Data.Char (isSpace)
import Data.List
import Data.List.Split
import Data.Ord
import System.Environment

strip :: String -> String
strip = f . f
   where f = reverse . dropWhile isSpace

overlay :: String -> String -> String
overlay ('2':xs) (y:ys)= y:(overlay xs ys)
overlay (x:xs) (_:ys)= x:(overlay xs ys)
overlay [] [] = []

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

solve :: Int -> Int -> String -> Int
solve width height = computeResult . fst . minimumBy (comparing snd) . map pairs . layers
  where pairs x = (x, count '0' x)
        computeResult x = count '1' x * count '2' x
        layers = chunksOf (width*height)

transparent :: Int -> Int -> String
transparent w h = take (w*h) (repeat '2')

replace :: String -> String
replace [] = []
replace ('1':xs) = '.':(replace xs)
replace ('0':xs) = ' ':(replace xs)

solve2 :: Int -> Int -> String -> String
solve2 width height input = intercalate "\n" . chunksOf width . replace $ result
  where result = foldl overlay (transparent width height) . chunksOf (width*height) $ input

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve 25 6 . strip $ content
  putStrLn . solve2 25 6 . strip $ content
