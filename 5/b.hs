import Data.List
import Data.List.Split
import Data.Maybe

type Interval = [Int]

from :: Interval -> Int
from interval = head interval

to :: Interval -> Int
to interval = last interval

combine :: [Interval] -> [Interval]
combine intervals =
  let sorted = sortBy (\a b -> compare (from a) (from b)) intervals
   in combine' (sorted !! 0) [] (drop 1 sorted)

combine' :: Interval -> [Interval] -> [Interval] -> [Interval]
combine' cur prefix [] = prefix ++ [cur]
combine' [f, t] prefix (x : xs) =
  if from x <= t
    then combine' [f, max t (to x)] prefix xs
    else combine' x (prefix ++ [[f, t]]) xs

main :: IO ()
main = do
  lines <- fmap lines (readFile "5/input.txt")
  let splitIdx = fromJust $ elemIndex "" lines
  let intervals = fmap (\x -> read <$> splitOn "-" x) $ take splitIdx lines :: [Interval]
  let result = sum $ (\x -> (to x) - (from x) + 1) <$> combine intervals
  print result
  print (combine intervals)
