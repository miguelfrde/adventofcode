{-# Language RecordWildCards #-}

import qualified Data.Set as Set
import Text.Regex.Base
import Text.Regex.PCRE
import System.Environment

type Triple = [Int]
data Moon = Moon { velocities :: Triple, positions :: Triple } deriving (Show, Eq)

delta :: Int -> Int -> Int
delta a b
  | b > a = 1
  | b < a = -1
  | otherwise = 0

advance :: Moon -> Moon
advance m@Moon{..} = m{ positions = zipWith (+) positions velocities }

updateGravity :: Moon -> Moon -> Moon
updateGravity m other = m{ velocities =  zipWith (+) (velocities m) deltas }
    where deltas = zipWith delta (positions m) (positions other)

potentialEnergy :: Moon -> Int
potentialEnergy = sum . map abs . positions

kineticEnergy :: Moon -> Int
kineticEnergy = sum . map abs . velocities

totalEnergy :: Moon -> Int
totalEnergy m = potentialEnergy m * kineticEnergy m

evolve :: Int -> [Moon] -> [Moon]
evolve steps = head . drop steps . iterate singleStep

singleStep :: [Moon] -> [Moon]
singleStep moons = [advance . updateAll moons $ a | a <- moons]
  where updateAll ms a = foldl updateGravity a ms

parse :: String -> Moon
parse input = Moon { velocities = [0,0,0], positions = [read x, read y, read z] }
    -- TODO: use a parse combinator
    where regex = "<x=(-?[0-9]+), y=(-?[0-9]+), z=(-?[0-9]+)>"
          [[_, x,y,z]] = input =~ regex :: [[String]]

timeToRepeat :: Int -> [[Moon]] -> Int
timeToRepeat i moons = timeToRepeat' Set.empty moons
    where relevant m = (positions m !! i, velocities m !! i)
          timeToRepeat' seen [] = Set.size seen
          timeToRepeat' seen (ms:next)
            | Set.member value seen = Set.size seen
            | otherwise = timeToRepeat' (Set.insert value seen) next
            where value = map relevant ms

solve :: Int -> String -> Int
solve steps input = sum . map totalEnergy . evolve steps $ moons
  where moons = map parse . lines $ input

solve2 :: String -> Int
solve2 input = foldl lcm timeX [timeY, timeZ]
  where moons = map parse . lines $ input
        timeX = timeToRepeat 0 . iterate singleStep $ moons
        timeY = timeToRepeat 1 . iterate singleStep $ moons
        timeZ = timeToRepeat 2 . iterate singleStep $ moons

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve 1000 $ content
  putStrLn . show . solve2 $ content
