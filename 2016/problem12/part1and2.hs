import System.Environment
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map


data Atom = Value Int | Register Char deriving (Show)
data Instruction
  = Copy Atom Atom
  | Increment Atom
  | Decrement Atom
  | JumpNotZero Atom Int
  deriving (Show)


type Program = [Instruction]
type State = Map.Map Char Int
type Register = Char


initialState :: State
initialState = Map.fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)]
--initialState = Map.fromList . map (\x -> (x, 0)) $ "abcd"  -- Use for problem1


parseInput :: String -> Program
parseInput = map parseLine . lines
  where parseLine line = case words line of
          ["cpy", value, register] -> Copy (strToAtom value) (strToAtom register)
          ["inc", register] -> Increment (strToAtom register)
          ["dec", register] -> Decrement (strToAtom register)
          ["jnz", value, offset] -> JumpNotZero (strToAtom value) (read offset :: Int)
        strToAtom x
          | all isDigit x = Value (read x :: Int)
          | otherwise = Register (head x)


evalProgram :: Program -> State
evalProgram instructions = eval 0 initialState
  where eval index state
          | index >= length instructions = state
          | otherwise = case (instructions !! index) of
              Increment (Register r) -> eval (index + 1) (update r (+1))
              Decrement (Register r) -> eval (index + 1) (update r (subtract 1))
              Copy (Register src) (Register dest) -> eval (index + 1) (copy (regVal src) dest)
              Copy (Value x) (Register dest) -> eval (index + 1) (copy x dest)
              JumpNotZero (Register r) offset -> jump (regVal r) offset
              JumpNotZero (Value x) offset -> jump x offset
          where
            regVal r = fromJust . Map.lookup r $ state
            update r f = Map.insert r (f . regVal $ r) state
            copy x r = Map.insert r x state
            jump val offset = if val /= 0 then eval (index + offset) state else eval (index + 1) state


solve :: String -> Maybe Int
solve = Map.lookup 'a' . evalProgram . parseInput


main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content
