import System.Environment
import Data.Ratio
import Text.JSON

numbers_in :: JSValue -> [Integer]
numbers_in (JSRational _ n) =  [numerator n]
numbers_in (JSArray xs) = concatMap numbers_in xs
numbers_in (JSObject xs)
    | any (\(k, v) -> is_str_red v) list = []
    | otherwise = concatMap (\(k, v) -> numbers_in v) list
    where list = fromJSObject xs
          is_str_red (JSString s) = (fromJSString s) == "red"
          is_str_red _ = False
numbers_in _ = []

solve :: String -> Integer
solve string = let json = decode string in case json of
    Ok okjson -> sum . numbers_in $ okjson
    Error _ -> 0


main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . solve $ content

