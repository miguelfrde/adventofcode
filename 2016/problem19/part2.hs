{-# LANGUAGE ViewPatterns #-}

import System.Environment
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Sequence ((|>), Seq, ViewL(..))
import qualified Data.Sequence as Seq


genNext :: Seq Int -> Seq Int
genNext s@(Seq.viewl -> (x :< xs)) = res |> x
  where res = Seq.deleteAt (Seq.length s `div` 2 - 1) xs


solve :: Int -> Seq.Seq Int
solve n = fromJust . find ((==1) . Seq.length) . iterate genNext $ Seq.fromList [1..n]


main :: IO ()
main = do
  [n] <- getArgs
  putStrLn . show . solve $ (read n :: Int)
