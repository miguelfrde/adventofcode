import System.Environment

encode :: String -> String
encode s = "\"" ++ encode' s ++ "\""
    where encode' ('"':cs) = "\\\"" ++ encode' cs
          encode' ('\\':cs) = "\\\\" ++ encode' cs
          encode' (c:cs) = c:encode' cs
          encode' "" = ""

diffString :: String -> Int
diffString s = (length . encode $ s) - (length s)

main :: IO ()
main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . sum . map diffString . lines $ content
