import Data.List.Split
import Data.Vector (Vector, (!), (//), toList, fromList, slice)
import System.Environment

type Program = Vector

data Op = Add | Mul

data State = State { position :: Int, program :: Program Int, finished :: Bool }

exec :: Op -> State -> State
exec op State { position = pos, program = prog, finished = f }
  = State { position = pos + 4, program = prog // [(out, value)], finished = f }
  where [a, b, out] = toList . slice (pos + 1) 3 $ prog
        x = prog ! a
        y = prog ! b
        value = case op of
            Add -> x + y
            Mul -> x * y

advance :: State -> State
advance state@(State { finished=True }) = state
advance state@State { position = i, program = p, finished = f } = case p ! i of
  1 -> exec Add state
  2 -> exec Mul state
  99 -> State { position = i, program = p, finished = True }

execute :: (Int, Int) -> Program Int -> [Int]
execute (noun, verb) p = toList . program . head . dropWhile (not . finished) . iterate advance $ initialState
  where initialState = State { position = 0, program = startup, finished = False }
        startup = p // [(1, noun), (2, verb)]

parseNumberList :: String -> [Int]
parseNumberList = map toInt . splitOn ","
  where toInt = read::String->Int

solve :: String -> Int
solve = head . execute (12, 2) . fromList . parseNumberList

solve2 :: String -> Int
solve2 input = 100 * noun + verb
  where alternatives = [(x, y) | x <- [0..99], y <- [0..99]]
        program = fromList . parseNumberList $ input
        result = head . dropWhile ((/=19690720) . head) . map (flip execute program) $ alternatives
        (_:noun:verb:_) = result

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve2 $ content
