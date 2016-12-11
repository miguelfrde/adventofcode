import System.Environment
import Data.List (sort)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map


type BotState = Map.Map Int (Maybe Int, Maybe Int)
type OutputState = Map.Map Int Int
data State = State BotState OutputState deriving (Show)


data Instruction
  = Value { value :: Int, node :: Node }
  | Give { bot :: Int, low :: Node, high :: Node }
  deriving (Show)

data Node = Output Int | Bot Int deriving (Show)


initialState :: State
initialState = State Map.empty Map.empty


isValueInstruction :: Instruction -> Bool
isValueInstruction (Value _ _) = True
isValueInstruction _ = False


botValues :: (Maybe Int, Maybe Int) -> [Int]
botValues (Just x, Just y) = [x, y]
botValues (Just x, Nothing) = [x]
botValues (Nothing, Just y) = [y]
botValues (Nothing, Nothing) = []


parseLine :: String -> Instruction
parseLine s = case words s of
  ["value", value, "goes", "to", "bot", bot] ->
    Value { value = read value, node = Bot (read bot) }
  ["bot", bot, "gives", "low", "to", "bot", low, "and", "high", "to", "bot", high] ->
    Give { bot = read bot, low = Bot (read low), high = Bot (read high) }
  ["bot", bot, "gives", "low", "to", "bot", low, "and", "high", "to", "output", high] ->
    Give { bot = read bot, low = Bot (read low), high = Output (read high) }
  ["bot", bot, "gives", "low", "to", "output", low, "and", "high", "to", "bot", high] ->
    Give { bot = read bot, low = Output (read low), high = Bot (read high) }
  ["bot", bot, "gives", "low", "to", "output", low, "and", "high", "to", "output", high] ->
    Give { bot = read bot, low = Output (read low), high = Output (read high) }


execInstruction :: Instruction -> State -> Maybe State
execInstruction (Value v (Bot b)) (State bs os) = case Map.lookup b bs of
  Just (x, Nothing) -> Just $ State (Map.insert b (x, Just v) bs) os
  Nothing -> Just $ State (Map.insert b (Just v, Nothing) bs) os
execInstruction (Value v (Output o)) (State bs os) = Just (State bs (Map.insert o v os))
execInstruction (Give bot lowNode highNode) state = case tryToGive bot lowNode state minimum of
    Nothing -> Nothing
    Just stateAfterLow -> tryToGive bot highNode stateAfterLow maximum

tryToGive src dst state@(State bs _) f = case Map.lookup src bs of
  Nothing -> Nothing
  Just values -> if (length . botValues $ values) < 2 then Nothing
                 else execInstruction (Value (f . botValues $ values) dst) state


runInstructions :: State -> [Instruction] -> State
runInstructions state instructions = runGiveInstructions' instructions state []
  where runGiveInstructions' [] state [] = state
        runGiveInstructions' [] state pending = runGiveInstructions' pending state []
        runGiveInstructions' (instr:instrs) state pending = case newState of
            Just newState' -> runGiveInstructions' instrs newState' pending
            Nothing -> runGiveInstructions' instrs state (instr:pending)
          where newState = execInstruction instr state


solve :: String -> Int
solve string = solveProblem2 $ finalState
  where finalState = runInstructions initialState . map parseLine . lines $ string
        solveProblem2 (State _ os) = product . catMaybes . map (\x -> Map.lookup x os) $ [0, 1, 2]


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
