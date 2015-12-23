import System.Environment
import Data.List
import Data.Maybe
import Data.String.Utils
import qualified Data.Map.Strict as Map

type Registers = Map.Map String Int

data State = State
    { registers :: Registers
    , currInstruction :: Int
    } deriving (Show)

data Instruction = Half String
                 | Triple String
                 | Increment String
                 | Jump Int
                 | JumpIfEven String Int
                 | JumpIfOne String Int
                 deriving (Show)

parseInstructionLine :: String -> Instruction
parseInstructionLine line =
    let parseNumber ('+':n) = read n
        parseNumber n = read n
    in case splitWs line of
        ["hlf", register] -> Half register
        ["tpl", register] -> Triple register
        ["inc", register] -> Increment register
        ["jmp", offset] -> Jump $ parseNumber offset
        ["jie", register, offset] -> JumpIfEven (init register) $ parseNumber offset
        ["jio", register, offset] -> JumpIfOne (init register) $ parseNumber offset

execInstruction :: [Instruction] -> State -> State
execInstruction instructions state =
    let instruction = instructions !! (currInstruction state)
        updateRegister r f = Map.insert r (f $ registers state Map.! r) $ registers state
        incInstruction = currInstruction state + 1
        jumpIfRegister r offset f
            | f (registers state Map.! r) = currInstruction state + offset
            | otherwise = incInstruction
    in case instruction of
        Half r -> state { registers = updateRegister r (`div` 2), currInstruction = incInstruction }
        Triple r -> state { registers = updateRegister r (*3), currInstruction = incInstruction }
        Increment r -> state { registers = updateRegister r (+1), currInstruction = incInstruction }
        Jump offset -> state { currInstruction = currInstruction state + offset }
        JumpIfEven r offset -> state { currInstruction = jumpIfRegister r offset even }
        JumpIfOne r offset -> state { currInstruction = jumpIfRegister r offset (==1) }

runProgram :: Registers -> [Instruction] -> State
runProgram initialRegs instructions = fromJust . find programFinished . iterate (execInstruction instructions) $ initialState
    where initialState = State { registers = initialRegs, currInstruction = 0 }
          programFinished state = currInstruction state >= length instructions

solve :: [(String, Int)] -> [String] -> State
solve initialState lineList = runProgram (Map.fromList initialState) $ instructions
    where instructions = map parseInstructionLine lineList

main = do
    [initial, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve [("a", read initial), ("b", 0)] . lines $ content
