import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Set as Set
import System.Environment


type Coordinate = (Int, Int)
data State = State String Coordinate deriving (Eq, Ord, Show)


isVault :: Coordinate -> Bool
isVault (3, 3) = True
isVault _ = False


nextMove :: Coordinate -> Char -> Coordinate
nextMove (x, y) 'U' = (x, y - 1)
nextMove (x, y) 'D' = (x, y + 1)
nextMove (x, y) 'L' = (x - 1, y)
nextMove (x, y) 'R' = (x + 1, y)


neighbors :: String -> State -> [State]
neighbors key (State path coord) = map (\d -> State (path ++ [d]) (nextMove coord d)) possibleDirections
  where importantHash = take 4 . show . md5 . pack $ (key ++ path)
        possibleDirections = map snd . filter (\(c, _) -> c `elem` "bcdef") . zip importantHash $ "UDLR"


findVault :: Coordinate -> String -> [String]
findVault initial key = bfs' [State "" initial] Set.empty []
  where bfs' [] explored solutions = solutions
        bfs' (current@(State path coord):rest) explored solutions
          | isVault coord = bfs' rest (Set.insert current explored) (path:solutions)
          | Set.member current explored = bfs' rest explored solutions
          | otherwise = bfs' (rest ++ nextStates) (Set.insert current explored) solutions
          where nextStates = filter validState . neighbors key $ current
                validState (State _ (x, y)) = x `elem` [0..3] && y `elem` [0..3]


solve :: String -> Int
solve = length . head . findVault (0, 0)


main :: IO ()
main = do
  [string] <- getArgs
  putStrLn . show . solve $ string
