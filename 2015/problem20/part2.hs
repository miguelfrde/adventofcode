import System.Environment
import Data.List

elvesGiftingHouse :: Int -> [Int]
elvesGiftingHouse house = elves
    where elves = nub . concat $ [[elf, house `div` elf] | elf <- [1..root], house `mod` elf == 0]
          root = floor . sqrt . fromIntegral $ house

presentsForHouse :: Int -> Int
presentsForHouse house = (11 *) . sum . filter (\elf -> house `div` elf <= 50) . elvesGiftingHouse $ house

solve :: Int -> Maybe Int
solve presents = findIndex (\x -> x >= presents) . map presentsForHouse $ [0..]

main = do
    [presents] <- getArgs
    putStrLn . show . solve $ read presents
