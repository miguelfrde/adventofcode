import System.Environment
import Data.Bits
import Data.Word (Word16)
import Data.Function.Memoize (memoize)
import Data.List.Split
import qualified Data.Map.Strict as Map

data Wire = Variable String | Value Word16 deriving (Show)

data Gate
    = Not Wire
    | And Wire Wire
    | Or Wire Wire
    | LShift Wire Int
    | RShift Wire Int
    | Simple Wire
    deriving (Show)

type Command = (Gate, Wire)

parseWire :: String -> Wire
parseWire wire = case reads wire :: [(Word16, String)] of
    [(num, "")] -> Value num
    _           -> Variable wire

parseCommand :: [String] -> Command
parseCommand tokens = case tokens of
    ["NOT", a, "->", b]       -> (Not (parseWire a), parseWire b)
    [a, "AND", b, "->", c]    -> (And (parseWire a) (parseWire b), parseWire c)
    [a, "OR", b, "->", c]     -> (Or (parseWire a) (parseWire b), parseWire c)
    [a, "LSHIFT", b, "->", c] -> (LShift (parseWire a) (read b), parseWire c)
    [a, "RSHIFT", b, "->", c] -> (RShift (parseWire a) (read b), parseWire c)
    [a, "->", b]              -> (Simple (parseWire a), parseWire b)

evaluateForWire :: (Map.Map String Gate) -> String -> Word16
evaluateForWire m wireName = eval' wireName
    where eval' wireName = case (m Map.! wireName) of
              (Simple wire) -> evalWire wire
              (Not wire) -> complement (evalWire wire)
              (And wire1 wire2) -> evalWire wire1 .&. evalWire wire2
              (Or wire1 wire2) -> evalWire wire1 .|. evalWire wire2
              (LShift wire1 bits) -> evalWire wire1 `shiftL` bits
              (RShift wire1 bits) -> evalWire wire1 `shiftR` bits
          evalWire (Variable name) = memoEval name
          evalWire (Value value) = value
          memoEval = memoize eval'

solve :: String -> [String] -> Word16
solve wire lineList = evaluateForWire wireMap wire
    where tokens line = splitOn " " line
          commands = map (parseCommand . tokens) lineList
          wireMap = foldl insertWireToMap Map.empty commands
          insertWireToMap m (gate, Variable name) = Map.insert name gate m

main = do
    [wire, f] <- getArgs
    content <- readFile f
    putStrLn . show . solve wire .  lines $ content
