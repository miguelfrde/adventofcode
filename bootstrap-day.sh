year="$1"
day=$(printf "%02d" $2)
directory="${year}/problem${day}"
template="import System.Environment

-- TODO
solve :: String -> Integer
solve string = 0

main :: IO ()
main = do
  [f] <- getArgs
  content <- readFile f
  putStrLn . show . solve $ content"

readme="# Day ${2}: <ADD TITLE>

<ADD DESCRIPTION>"

mkdir "$directory"
mkdir "$directory/tests"
ln -s Makefile $directory/Makefile
echo "$readme" > $directory/README.md
echo "$template" >  $directory/part1.hs
echo "$template" > $directory/part2.hs
