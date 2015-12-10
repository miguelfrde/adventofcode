import System.Environment

decode :: String -> String
decode ('"':cs) = decode cs
decode ('\\':'"':cs) = '\"':decode cs
decode ('\\':'\\':cs) = '\\':decode cs
decode ('\\':'x':a:b:cs) = 'x':decode cs
decode (c:cs) = c:(decode cs)
decode "" = ""

diffString :: String -> Int
diffString s = (length s) - (length . decode $ s)

main :: IO ()
main = do
    [f] <- getArgs
    content <- readFile f
    putStrLn . show . sum $ map diffString (lines content)
