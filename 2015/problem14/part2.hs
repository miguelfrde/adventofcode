import System.Environment;
import Data.List
import GHC.Exts
import Data.List.Split

data ReindeerState = Flying | Resting deriving (Show, Eq);

data Reindeer = Reindeer
    { name :: String
    , speed :: Int
    , flyTime :: Int
    , restTime :: Int
    , distance :: Int
    , state :: ReindeerState
    , timeLeftAtState :: Int
    , points :: Int
    } deriving (Show, Eq)

instance Ord Reindeer where
    r1 `compare` r2 = points r1 `compare` points r2

compareByDistance :: Reindeer -> Reindeer -> Ordering
compareByDistance r1 r2 = distance r1 `compare` distance r2

parseLine :: String -> Reindeer
parseLine s = case splitOn " " s of
    [name, _, _, speed, _, _, flyTime, _, _, _, _, _, _, restTime, _] ->
        Reindeer { name = name
                 , speed = read speed
                 , flyTime = read flyTime
                 , restTime = read restTime
                 , distance = 0
                 , state = Flying
                 , timeLeftAtState = read flyTime
                 , points = 0
                 }

moveOneSecond :: [Reindeer] -> [Reindeer]
moveOneSecond reindeers = reindeersWithPoints
    where reindeers' = map updateReindeer reindeers
          maxDistance = distance . maximumBy compareByDistance $ reindeers'
          addPoints r
            | distance r == maxDistance = r { points = (points r) + 1 }
            | otherwise = r
          reindeersWithPoints = map addPoints reindeers'
          updateReindeer r
            | state r == Flying && timeLeftAtState r > 0 =
                r { distance = speed r + distance r, timeLeftAtState = timeLeftAtState r - 1 }
            | state r == Flying && timeLeftAtState r <= 0 =
                updateReindeer r { state = Resting, timeLeftAtState = restTime r }
            | state r == Resting && timeLeftAtState r > 0 =
                r { timeLeftAtState = timeLeftAtState r - 1 }
            | state r == Resting && timeLeftAtState r <= 0 =
                updateReindeer r { state = Flying, timeLeftAtState = flyTime r }

solve :: Int -> [String] -> Int
solve seconds lineList = points . maximum . (!! seconds) . iterate moveOneSecond $ reindeers
    where reindeers = map parseLine lineList

main = do
    [secs, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve (read secs) . lines $ content
