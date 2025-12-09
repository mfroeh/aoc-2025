import Data.List.Split
import Data.List

type Point = (Int, Int)

parsePoint :: String -> Point
parsePoint line = (s!!0, s!!1) where s = read <$> splitOn "," line :: [Int]

pairwise :: [a] -> [(a, a)]
pairwise [] = []
pairwise (x:xs) = [(x, y) | y <- xs] ++ pairwise xs

-- Manhattan distance gives us the sum of the length of both sides
dist :: Point -> Point -> Int
dist (p1, p2) (q1, q2)= abs (p1 - q1) + abs (p2 - q2)

main :: IO ()
main = do
  points <- (readFile "9/input.txt") >>= (pure . (fmap parsePoint) . lines)
  let pairs = pairwise points
  let (x, y) = maximumBy (\(a, b) (c, d) -> (dist a b) `compare` (dist c d)) pairs
  let res = (abs (fst x - fst y) + 1) * (abs (snd x - snd y) + 1)
  print res
