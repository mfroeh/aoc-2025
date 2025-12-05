import Data.List
import Data.List.Split
import Data.Maybe

type Interval = [Int]

isFresh :: [Interval] -> Int -> Bool
isFresh [] _ = False
isFresh ([from, to] : xs) n = (from <= n && n <= to) || isFresh xs n

main :: IO ()
main = do
  lines <- fmap lines (readFile "5/input.txt")
  let splitIdx = fromJust $ elemIndex "" lines
  let intervals = fmap (\x -> read <$> splitOn "-" x) $ take splitIdx lines :: [[Int]]
  let targets = fmap read $ drop (splitIdx + 1) lines :: [Int]
  let result = length $ filter (\x -> isFresh intervals x) targets
  print result
